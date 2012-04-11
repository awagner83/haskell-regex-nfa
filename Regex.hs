module Regex (Match(..), Regex(..), matches) where

import Control.Monad (join)


data Match = LiteralChar Char
           | AnyChar
           | Range Char Char
           deriving (Eq, Show)
data Regex = Step Match Regex
           | Split Regex Regex
           | MatchEnd
           deriving (Eq, Show)


-- | Does string match given regular expression?
matches :: String -> Regex -> Bool
matches s r = checkNext s [r]
    where checkNext xs = go xs . followSplits
          go [] rs     = any (MatchEnd ==) rs
          go _  []     = False
          go (x:xs) rs = checkNext xs $ followSteps x rs

-- | Advance state for all 'Splits' found in list of Regexes
followSplits :: [Regex] -> [Regex]
followSplits = join . map followSplit
    where followSplit (Split l r) = [l, r]
          followSplit r           = [r]

-- | Advance state for all 'Steps', pruning those that are no longer relevant
followSteps :: Char -> [Regex] -> [Regex]
followSteps c = join . map followStep
    where followStep (Step m r) | m `matchesChar` c = [r]
                                | otherwise         = []
          followStep MatchEnd                       = []
          followStep r                              = [r]

-- | Check if char matches given Match data
matchesChar :: Match -> Char -> Bool
matchesChar (LiteralChar x) c = x == c
matchesChar (Range l r) c     = l <= c && c <= r
matchesChar AnyChar         _ = True

