module Compiler where

import Control.Monad
import Data.Maybe
import Data.List (foldl1', groupBy)
import Text.Parsec ((<|>), many1)
import qualified Text.Parsec as P
import Text.Parsec.String
import Regex


data Token = TokenMatch (Regex -> Regex)
           | TokenOperator ((Regex -> Regex) -> (Regex -> Regex))

-- | Construct Regex from given string
compile :: String -> Regex
compile s = case (P.parse regex "regex" s) of
                Left err -> error $ show err
                Right r  -> r

-- | Main regex parser entry point
regex :: Parser Regex
regex = do
    steps <- many1 token
    return $ (foldRegex $ applyTokenOp steps) MatchEnd

-- Tokens
tokenMatch = return . TokenMatch . Step
token = alternation <|> zeroOrMore <|> anyChar <|> escapedChar <|> literalChar
anyChar = P.char '.' >> (tokenMatch AnyChar)
escapedChar = P.char '\\' >> P.anyChar >>= tokenMatch . LiteralChar
literalChar = P.anyChar >>= tokenMatch . LiteralChar
zeroOrMore = P.char '*' >> (return $ TokenOperator repeating)
alternation = do
    P.char '|'
    (TokenMatch r) <- token
    return (TokenOperator $ split r)

-- | Apply token operators and return regex
applyTokenOp :: [Token] -> [(Regex -> Regex)]
applyTokenOp = join . map (go . reverse) . groupBy (\a b -> isOperator b)
    where go (TokenOperator o:TokenMatch t:ts) = go ((TokenMatch $ o t):ts)
          go ts = (reverse . catMaybes . map unpackToken) ts

foldRegex :: [(Regex -> Regex)] -> (Regex -> Regex)
foldRegex = foldl1' (.)

unpackToken :: Token -> Maybe (Regex -> Regex)
unpackToken (TokenMatch t) = Just t
unpackToken _              = Nothing

isOperator (TokenOperator _) = True
isOperator _                 = False

repeating :: (Regex -> Regex) -> Regex -> Regex
repeating left right = let r = Split (left r) right in r

split :: (Regex -> Regex) -> (Regex -> Regex) -> (Regex -> Regex)
split right left joined = Split (left joined) (right joined)

