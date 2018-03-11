module Codewars.Kata.PileOfCubes where

findNb :: Integer -> Integer
findNb m = findNb' 0 1
 where
  findNb' prevVolume n
    | fromInteger m == volume = n
    | fromInteger m < volume = -1
    | otherwise = findNb' volume (n+1)
    where
      volume = prevVolume + (n^3)
