module Codewars.Kata.XO where

import Data.Char


numberOfChar :: Char -> String -> Int
numberOfChar x = length . filter (==x) . map toLower

-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
xo :: String -> Bool
xo str = numberOfChar 'x' str == numberOfChar 'o' str
