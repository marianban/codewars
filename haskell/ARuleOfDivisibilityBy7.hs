module Codewars.G964.Seven where

seven :: Integer -> (Integer, Int)
seven m = seven' (m, 0) where
  seven' :: (Integer, Int) -> (Integer, Int)
  seven' (m', step)
    | m' < 100 = (m', step)
    | otherwise = seven' (firstDigits - (lastDigit * 2), step + 1) where
      mAsString = show m'
      firstDigits = read $ init mAsString
      lastDigit = read [last mAsString]
