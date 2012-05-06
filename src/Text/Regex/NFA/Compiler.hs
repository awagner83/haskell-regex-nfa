module Text.Regex.NFA.Compiler (compile) where

import Control.Applicative ((<$>))
import Data.List (foldl1')
import Text.Parsec hiding (between)
import Text.Parsec.String

import Text.Regex.NFA.Regex


compile :: Monad m => String -> m Regex
compile s = either (fail . show) closeRegex parseRegex
      where closeRegex rx = return $ rx MatchEnd
            parseRegex    = parse regex s "Regular Expression"

-- | Top-level parser & alternatives in order of precedence.
--   This is broken up so we don't get caught in a never-ending parse.
regex       = foldMany1 level1
level1      = verticalbar <|> level2
level2      = asterisk <|> plus <|> qmark <|> level3
level3      = group <|> charset <|> dot <|> escaped <|> character

-- | Token Parsers
character   = Step <$> literal
escaped     = char '\\' >> Step . LiteralChar <$> anyChar
dot         = char '.'  >> return (Step AnyChar)
qmark       = postfix '?' (\l r -> Split (l r) r)
asterisk    = postfix '*' (\l r -> let s = Split (l s) r in s)
plus        = postfix '+' (\l r -> let s = Split (l s) r in l s)
group       = between '(' regex ')'
charset     = Step . MatchSet <$> between '[' (many1 $ range <|> literal) ']'
range       = infixop '-' anyChar Range
literal     = LiteralChar <$> noneOf "().|[]"
verticalbar = infixop '|' (foldMany1 level2) (\l r j -> Split (l j) (r j))

-- | Helper for constructing postfix operator parsers
postfix op f = try (level3 >>= (char op >>) . return . f)

-- | Find many (but at least one) regex tokens and fold them into one
foldMany1 parser = foldl1' (.) <$> many1 parser

-- | Helper for finding a token/pattern between two chars
between l token r = do
    char l
    t <- token
    char r
    return t

-- | Helper for constructing infix operators
infixop op argParser f = try $ do
    l <- argParser
    char op
    r <- argParser
    return $ f l r

