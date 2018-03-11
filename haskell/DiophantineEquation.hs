module Codewars.Kata.Dioph where

solequa :: Integer -> [(Integer, Integer)]
solequa n = [(x, y) | x <- numbers, y <- numbers, (x - 2*y) * (x + 2*y) == n] where
  numbers = [0..n]
