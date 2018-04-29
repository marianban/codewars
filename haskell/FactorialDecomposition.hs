module FactorialDecomposition.Kata (decomp) where

import Data.Maybe

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factors :: Integer -> [(Integer, Integer)]
factors n = [(x, n `div` x) | x <- [2..n'] ++ [n], x == 2 || odd x, n `mod` x == 0] where
  n' = ceiling $ sqrt $ fromIntegral n

primeFactors :: Integer -> [Integer]
primeFactors n = [x | (x, _) <- factors n, isPrime x]

someOrNothing :: [a] -> Maybe [a]
someOrNothing [] = Nothing
someOrNothing xs = Just xs

isPrime :: Integer -> Bool
isPrime n = (fst $ head $ factors $ n) == n

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = Just $ head xs

decomp :: Int -> [Integer]
decomp n = primeFactors $ factorial $ toInteger n
