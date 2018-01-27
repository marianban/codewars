module Codewars.Kata.Deletion where

deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n = foldl reducer [] lst where
  reducer xs x
    | (length $ (x==) `filter` xs) < n = xs ++ [x]
    | otherwise = xs
