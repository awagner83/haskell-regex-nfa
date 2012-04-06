module Regex where

import Control.Monad (join)


data Match = LiteralChar Char | AnyChar deriving (Eq, Show)
data Regex = Step Match Regex
           | Split Regex Regex
           | MatchEnd
           deriving (Eq, Show)


-- | Does string match given regular expression?
matches :: String -> Regex -> Bool
matches s r = go s [r]
    where go [] rs     = any (MatchEnd ==) rs
          go _  []     = False
          go (x:xs) rs = go xs (advanceState x rs)

-- | Advance one step in concurrent tracking of states
advanceState :: Char -> [Regex] -> [Regex]
advanceState c = join . map go
    where go (Step m r) | m `matchesChar` c = [r]
                        | otherwise         = []
          go (Split l r)                    = join [go l, go r]
          go MatchEnd                       = []

-- | Check if char matches given Match data
matchesChar :: Match -> Char -> Bool
matchesChar (LiteralChar x) c = x == c
matchesChar AnyChar         _ = True

