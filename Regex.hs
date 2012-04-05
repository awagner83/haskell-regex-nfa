module Regex where

import Control.Monad (join)


data Match = LiteralChar Char | AnyChar deriving (Eq)
data Regex = Step Match Regex
           | Split Regex Regex
           | MatchEnd
           deriving (Eq)


-- | Does string match given regular expression?
matches :: String -> Regex -> Bool
matches s r = go s [r]
    where go [] rs = any (MatchEnd ==) rs
          go _  [] = False
          go (x:xs) rs = go xs (advanceState x rs)


-- | Advance one step in concurrent tracking of states
advanceState :: Char -> [Regex] -> [Regex]
advanceState c = join . map go
    where go (Step m r) | m `matchesChar` c = [r]
                        | otherwise         = []
          go (Split l r) = join [go l, go r]
          go MatchEnd    = []


matchesChar :: Match -> Char -> Bool
matchesChar (LiteralChar x) c = x == c
matchesChar AnyChar         _ = True


-- | Build repeating regex sequence
repeating :: (Regex -> Regex) -> Regex -> Regex
repeating left right = let r = Split (left r) right in r


-- | Literal regex match
literal :: String -> Regex -> Regex
literal (x:[]) r = Step (LiteralChar x) r
literal (x:xs) r = Step (LiteralChar x) (literal xs r)


