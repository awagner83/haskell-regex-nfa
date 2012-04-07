
import Compiler (compile)
import Data.Either (lefts)

import Regex


test = [ matchE   "a"                     "a"
       , matchE   "."                     "a"
       , matchE   ".*"                    "aa"
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
       , matchE   ".*a"                   "ca"
       , matchE   ".*a"                   "abca"
       , matchE   "a(bb)*a"               "abba"
       , matchE   "a(bb)*a"               "aa"
       , matchE   "a(bb)*a"               "abbbba"
       , nmatchE  "a(bb)*a"               "aba"
       , matchE   "(bob)|(fred)"          "bob"
       , matchE   "(bob)|(fred)"          "fred"
       , nmatchE  "(bob)|(fred)"          "bill"
       , matchE   "(bob)|(fred) smith"    "fred smith"
       , matchE   "(bob)|(fred) smith"    "bob smith"
       ]

main = print $ lefts test

match = (flip matches) . compile
nomatch r s = not (match r s)
matchE r s | match r s = Right ()
           | otherwise = Left (r, s)
nmatchE r s | match r s = Left (r, s)
            | otherwise = Right ()

