module Kata where

and' :: [Bool] -> Bool
and' [] = False
and' xs = and xs

isOddHeavy :: [Int] -> Bool
isOddHeavy xs
 | and' $ map odd xs = True
 | otherwise = and' [ x > y | x <- filter odd xs, y <- filter even xs]
