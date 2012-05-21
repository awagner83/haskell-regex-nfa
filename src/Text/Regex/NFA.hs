module Text.Regex.NFA (compile) where

import Control.Applicative ((<$>))
import Data.List (foldl1')
import Text.Parsec hiding (between, token)
import Text.Parsec.String

import qualified Data.Match as M
import Data.NFA


type Regex = NFA Char

compile :: Monad m => String -> m Regex
compile s = either (fail . show) closeRegex parseRegex
      where closeRegex rx = return $ rx MatchEnd
            parseRegex    = parse regex "Regular Expression" s

-- | Top-level parser & alternatives in order of precedence.
--   This is broken up so we don't get caught in a never-ending parse.
regex, level1, level2, level3 :: Parser (Regex -> Regex)
regex       = foldMany1 level1
level1      = verticalbar <|> level2
level2      = asterisk <|> plus <|> qmark <|> level3
level3      = group <|> charset <|> dot <|> escaped <|> character

-- | Token Parsers
character, escaped, dot, qmark, asterisk, plus, group, charset,
    verticalbar :: Parser (Regex -> Regex)
character   = Step <$> literal
escaped     = char '\\' >> Step . M.Exactly <$> anyChar
dot         = char '.'  >> return (Step M.Everything)
qmark       = postfix '?' (\l r -> Split (l r) r)
asterisk    = postfix '*' (\l r -> let s = Split (l s) r in s)
plus        = postfix '+' (\l r -> let s = Split (l s) r in l s)
group       = between '(' regex ')'
charset     = Step . M.Any <$> between '[' (many1 $ range <|> literal) ']'
verticalbar = infixop '|' (foldMany1 level2) (\l r j -> Split (l j) (r j))

-- | Match parsers
literal, range :: Parser (M.Match Char)
literal = M.Exactly <$> noneOf "().|[]"
range   = infixop '-' anyChar M.between

-- | Helper for constructing postfix operator parsers
postfix :: Char                                     -- `postfix operator
        -> ((Regex -> Regex) -> (Regex -> Regex))   -- `fn of two partial regex
        -> Parser (Regex -> Regex)                  -- `return partial regex
postfix op f = try (level3 >>= (char op >>) . return . f)

-- | Find many (but at least one) regex tokens and fold them into one
foldMany1 :: Parser (Regex -> Regex) -> Parser (Regex -> Regex)
foldMany1 parser = foldl1' (.) <$> many1 parser

-- | Helper for finding a token/pattern between two chars
between :: Char -> Parser a -> Char -> Parser a
between l token r = do
    _ <- char l
    t <- token
    _ <- char r
    return t

-- | Helper for constructing infix operators
infixop op argParser f = try $ do
    l <- argParser
    _ <- char op
    r <- argParser
    return $ f l r

