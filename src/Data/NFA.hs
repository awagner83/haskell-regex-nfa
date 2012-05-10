{- |
 - NFA type and related utilities.
 -
 - This can be thought of as an upgrade to the Match type, in that it
 - not only compares values to some possible values, but has sequences, and
 - branches (splits) of comparisons.
 -
 - This can be used for, most notably, regular expressions.
 -}

module Data.NFA (NFA(..), matches) where

import Data.Match (Match, (~=))


data NFA a = Step (Match a) (NFA a)
             | Split (NFA a) (NFA a)
             | MatchEnd
             deriving (Eq, Show)
data State a = State [a] [NFA a]


-- | Does string match given regular expression?
matches :: Ord a => [a] -> NFA a -> Bool
matches s = isMatch . until isComplete runNFA . State s . expandSplit
    where runNFA   (State (x:xs) rs) = State xs (advanceStates x rs)
          isComplete (State    xs  rs) = null xs || null rs
          isMatch    (State     _  rs) = any (MatchEnd ==) rs

-- | Advance regex states
advanceStates :: Ord a => a -> [NFA a] -> [NFA a]
advanceStates c = concat . map step
    where step (Step m r) | c ~= m = expandSplit r
          step _                   = []

-- | Advance new split states
expandSplit :: NFA a -> [NFA a]
expandSplit (Split l r) = [l, r]
expandSplit r           = [r]

