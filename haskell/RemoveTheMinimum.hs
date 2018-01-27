module Codewars.Kata.RemoveSmallest where

removeSmallest :: [Int] -> [Int]
removeSmallest xs = foldr filterFirstSmallest [] xs where
  smallest = minimum xs
  numberOfSmallests ys = length $ filter (== smallest) ys
  filterFirstSmallest x' xs'
    | x' /= smallest = x' : xs'
    | x' == smallest && ((numberOfSmallests xs) - (numberOfSmallests xs') > 1) = x' : xs'
    | otherwise = xs'
