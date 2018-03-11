module Kata (decipherThis) where

import Data.Char (isDigit, chr)

firstLetter :: String -> Char
firstLetter = chr . read . takeWhile isDigit

decriptRest :: String -> String
decriptRest [] = []
decriptRest [x] = [x]
decriptRest rest = [last rest] ++ (tail . init) rest ++ [head rest]

decriptWord :: String -> String
decriptWord word = firstLetter word : decriptRest (dropWhile isDigit word)

decipherThis :: String -> String
decipherThis message = unwords $ map decriptWord (words message)
