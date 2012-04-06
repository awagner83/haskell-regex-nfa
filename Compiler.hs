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
    return $ (foldl1' (.) steps) MatchEnd

-- Tokens
token = anyChar <|> escapedChar <|> literalChar
anyChar = P.char '.' >> (return $ Step AnyChar)
escapedChar = P.char '\\' >> P.anyChar >>= return . Step . LiteralChar
literalChar = P.anyChar >>= return . Step . LiteralChar

-- Token Operators
oneOrMore :: Parser Token
oneOrMore = P.char '+' >> (return $ TokenOperator repeating)


-- | Apply token operators and return regex
applyTokenOp :: [Token] -> (Regex -> Regex)
applyTokenOp = foldl1' (.) . join . map (go . reverse) . groupBy (\a b -> isOperator b)
    where go (TokenOperator o:TokenMatch t:ts) = go ((TokenMatch $ o t):ts)
          go ts = (reverse . catMaybes . map unpackToken) ts

unpackToken :: Token -> Maybe (Regex -> Regex)
unpackToken (TokenMatch t) = Just t
unpackToken _              = Nothing

-- | Is token an operator?
isOperator (TokenOperator _) = True
isOperator _                 = False

-- | Build repeating regex sequence
repeating :: (Regex -> Regex) -> Regex -> Regex
repeating left right = let r = Split (left r) right in r

