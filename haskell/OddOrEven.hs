module Kata.OddOrEven where

oddOrEven :: [Int] -> String
oddOrEven xs = if odd $ sum xs then "odd" else "even"
