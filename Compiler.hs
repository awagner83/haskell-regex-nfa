module Compiler where

import Control.Applicative ((<$>))
import Data.List (foldl1')
import Text.Parsec
import Text.Parsec.String

import Regex


compile :: String -> Regex
compile s = case (parse regex "regex 2000" s) of
                Left err -> error $ show err
                Right rx -> rx MatchEnd

-- | Token Parsers
regex       = foldl1' (.) <$> many1 segment
segment     = verticalbar <|> asterisk <|> qmark <|> step
step        = dot <|> escaped <|> character
character   = Step . LiteralChar <$> anyChar
escaped     = char '\\' >> character
dot         = char '.'  >> return (Step AnyChar)
qmark       = postfix '?' (\l r -> Split (l r) r)
asterisk    = postfix '*' (\l r -> let s = Split (l s) r in s)
verticalbar = try $ do
    l <- step
    char '|'
    r <- step
    return $ (\joinTo -> Split (l joinTo) (r joinTo))

-- | Helper for constructing postfix operator parsers
postfix op f = try (step >>= (char op >>) . return . f)

