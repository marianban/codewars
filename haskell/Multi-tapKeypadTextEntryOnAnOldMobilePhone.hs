module Haskell.Codewars.KeypadEntry where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char (toUpper)

keypads :: [String]
keypads =
  [
    "1",
    "ABC2",
    "DEF3",
    "GHI4",
    "JKL5",
    "MNO6",
    "PQRS7",
    "TUV8",
    "WXYZ9",
    "*",
    " 0",
    "#"
  ]

findKeypad :: Char -> String
findKeypad char = head $ filter (elem char) keypads

getPressCount:: Char -> String -> Int
getPressCount char keypad = 1 + fromJust (elemIndex char keypad)

presses :: String -> Int
presses = foldr countPresses 0 where
  countPresses char totalPressCount = totalPressCount + getPressCount char' (findKeypad char') where
    char' = toUpper char
