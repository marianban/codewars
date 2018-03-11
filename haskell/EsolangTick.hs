{-# LANGUAGE MultiWayIf #-}
module Haskell.SylarDoom.Tick where

import Data.Char

decodeCell :: String -> Int
decodeCell tape = length (takeWhile (=='+') tape) `mod` 255

interpreter :: String -> String
interpreter tape = map chr (interperter' tape [] 0 0 [])

interperter' :: String -> [Int] -> Int -> Int -> [Int] -> [Int]
interperter' [] _ _ _ output = output
interperter' (x : tape) cells index cell output
  | x == '+' = interperter' tape cells index ((cell + 1) `mod` 255) output
  | x == '*' = if
    | index > 0 -> interperter' tape cells index cell (output ++ [cell])
    | otherwise -> interperter' tape (cells ++ [cell]) index cell (output ++ [cell])
  | x == '<' = let newIndex = (index + 1) `mod` length cells in if
    | newIndex == 0 -> interperter' tape cells newIndex 0 output
    | otherwise -> interperter' tape cells newIndex (cells !! ((length cells - 1) - (index + 1))) output
  | x == '>' = if
    | index == 0 -> interperter' tape cells index 0 output
    | otherwise -> interperter' tape cells (index - 1) (cells !! ((length cells - 1) - (index + 1))) output
  | otherwise = output
