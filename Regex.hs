
import Data.List (foldl1)


data Regex = Step Char Regex
           | Split Regex Regex
           | Match

-- | Does string match given regular expression?  This is a bit naive at the
--   the moment.  Need to step through splits concurrently
matches :: String -> Regex -> Bool
matches [] Match = True
matches [] _     = False
matches _  Match = False
matches (x:xs) (Step c r) | c == x = matches xs r
                          | otherwise = False
matches xs (Split l r) = matches xs l || matches xs r

-- | Build repeating regex sequence
repeating :: (Regex -> Regex) -> Regex -> Regex
repeating left right = let r = Split (left r) right in r

-- | Literal regex match
literal :: String -> Regex -> Regex
literal (x:[]) r = Step x r
literal (x:xs) r = Step x (literal xs r)


{- Since we don't have a regex compiler yet, here are some prebuild expressions
   to play with -}

-- regex: ab+a
m1 = Step 'a' $ repeating (Step 'b') $ Step 'a' $ Match

-- regex: a(bb)+a
m2 = Step 'a' $ repeating (Step 'b' . Step 'b') $ Step 'a' $ Match

-- regex: bob|fred
m3 = Split (literal "bob" $ Match) (literal "fred" $ Match)

