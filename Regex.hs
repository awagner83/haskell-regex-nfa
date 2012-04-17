module Regex (Match(..), Regex(..), matches) where


data Match = LiteralChar Char | AnyChar | Range Char Char | MatchSet [Match]
           deriving (Eq, Show)
data Regex = Step Match Regex | Split Regex Regex | MatchEnd
           deriving (Eq, Show)
data State = State String [Regex]


-- | Does string match given regular expression?
matches :: String -> Regex -> Bool
matches s = isMatch . until isComplete runRegex . State s . expandSplit
    where runRegex   (State (x:xs) rs) = State xs (advanceStates x rs)
          isComplete (State    xs  rs) = null xs || null rs
          isMatch    (State     _  rs) = any (MatchEnd ==) rs

-- | Advance regex states
advanceStates :: Char -> [Regex] -> [Regex]
advanceStates c = concat . map step
    where step (Step m r) | charMatch c m = expandSplit r
          step _                          = []

-- | Advance new split states
expandSplit :: Regex -> [Regex]
expandSplit (Split l r) = [l, r]
expandSplit r           = [r]

-- | Check if char matches given Match data
charMatch :: Char -> Match -> Bool
charMatch c (LiteralChar x) = x == c
charMatch c (Range l r)     = l <= c && c <= r
charMatch c (MatchSet xs)   = any (charMatch c) xs
charMatch _ AnyChar         = True

