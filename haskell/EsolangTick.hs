{-# LANGUAGE MultiWayIf #-}
module Haskell.SylarDoom.Tick where

import Data.Char

interpreter :: String -> String
interpreter tape = interpreter' tape [0] 0 []

replaceAt :: [Int] -> Int -> Int -> [Int]
replaceAt [] _ _ = []
replaceAt cells index cell = replaceAt' cells 0 [] where
  replaceAt' :: [Int] -> Int -> [Int] -> [Int]
  replaceAt' [] _ output = output
  replaceAt' (c : rest) i output
    | i == index = output ++ [cell] ++ rest
    | otherwise = replaceAt' rest (i + 1) (output ++ [c])

interpreter' :: String -> [Int] -> Int -> String -> String
interpreter' _ [] _ _ = error "Cells can't be empty"
interpreter' [] _ _ output = output
interpreter' (x : tape) cells index output
   | x == '+' = interpreter' tape (replaceAt cells index (((cells !! index) + 1) `mod` 255)) index output
   | x == '*' = interpreter' tape cells index (output ++ [chr (cells !! index)])
   | (x == '>') && (index == (length cells - 1)) = interpreter' tape (cells ++ [0]) (index + 1) output
   | x == '>' = interpreter' tape cells (index + 1) output
   | (x == '<') && (index == 0) = interpreter' tape (0 : cells) index output
   | x == '<'= interpreter' tape cells (index - 1) output
   | otherwise = interpreter' tape cells index output
