{- Run list of primitive tests.  Failing tests are displayed. -}

import Compiler (compile)
import Data.Either (lefts)

import Regex

test :: [Either (String, String) ()]
test = [ matchE   "a"                     "a"
       , matchE   "."                     "a"
       , matchE   "a?"                    ""
       , matchE   ".*"                    "aa"
       , matchE   ".*"                    ""
       , nmatchE  "."                     "aa"
       , nmatchE  "a"                     "b"
       , matchE   "a|b"                   "a"
       , matchE   "a|b"                   "b"
       , nmatchE  "a|b"                   "c"
       , matchE   "a?"                    "a"
       , matchE   "a?"                    ""
       , nmatchE  "a?"                    "b"
       , matchE   "a?b"                   "ab"
       , matchE   "a?b"                   "b"
       , matchE   ".*"                    "a"
       , matchE   ".*a"                   "ca"
       , matchE   ".*a"                   "abca"
       , matchE   "a(bb)*a"               "abba"
       , matchE   "a(bb)*a"               "aa"
       , matchE   "a(bb)*a"               "abbbba"
       , nmatchE  "a(bb)*a"               "aba"
       , matchE   "bob|fred"              "bob"
       , matchE   "(bob)|(fred)"          "bob"
       , matchE   "(bob|fred)"            "fred"
       , nmatchE  "(bob)|(fred)"          "bill"
       , matchE   "(bob|fred) smith"      "fred smith"
       , matchE   "(bob|fred) smith"      "bob smith"
       , matchE   "a+"                    "a"
       , matchE   "a+"                    "aa"
       , matchE   "a+"                    "aaa"
       , nmatchE  "a+"                    ""
       , matchE   "[a-z]"                 "a"
       , matchE   "[a-z]"                 "b"
       , nmatchE  "[a-z]"                 "2"
       , matchE   "[A-Z123]+"             "ABC12"
       , nmatchE  "[A-Z123]+"             "ABC15"
       , matchE   "[0-9-]+"               "555-1212"
       ]

main :: IO ()
main = print $ lefts test

match :: String -> String -> Bool
match = (flip matches) . compile

matchE, nmatchE :: String -> String -> Either (String, String) ()
matchE r s | match r s = Right ()
           | otherwise = Left (r, s)
nmatchE r s | match r s = Left (r, s)
            | otherwise = Right ()

