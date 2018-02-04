module Codewars.Kata.TenMinuteWalk where

isShorterThan :: Int -> [a] -> Bool
isShorterThan x xs = length (take x xs) < x

isLongerThan :: Int -> [a] -> Bool
isLongerThan x xs = length (take (x + 1) xs) > x

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (x==) xs

isValidWalk :: String -> Bool
isValidWalk walk
  | isShorterThan 10 walk || isLongerThan 10 walk = False
  | otherwise = count 'n' walk == count 's' walk && count 'w' walk == count 'e' walk
