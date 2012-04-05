
import Regex

{- Since we don't have a regex compiler yet, here are some prebuild expressions
   to play with -}

matchChar = Step . LiteralChar

-- regex: ab+a
m1 = matchChar 'a' $ repeating (matchChar 'b') $ matchChar 'a' $ MatchEnd

-- regex: a(bb)+a
m2 = matchChar 'a' 
     $ repeating (matchChar 'b' . matchChar 'b') $ matchChar 'a' $ MatchEnd

-- regex: bob|fred
m3 = Split (literal "bob" $ MatchEnd) (literal "fred" $ MatchEnd)

m4 = repeating (Step AnyChar) $ literal ".txt" $ MatchEnd


test = [ "a" `matches` (matchChar 'a' $ MatchEnd)
       , not $ "aa" `matches` (matchChar 'a' $ MatchEnd)
       , "aba" `matches` m1
       , "abba" `matches` m2
       , "bob" `matches` m3
       , "fred" `matches` m3
       , not $ "frank" `matches` m3
       , not $ "freddy" `matches` m3
       , "foo.txt" `matches` m4
       , not $ "foo.html" `matches` m4]

main = print test

