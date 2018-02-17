module Codewars.Kata.PP (isPP) where

import Data.Maybe

isPP i = listToMaybe [(m, k) | m <- [2..base], k <- [2..(getExponent m)],  m^k == i] where
  base = fromIntegral $ ceiling $ sqrt (fromInteger i)
  getExponent m = ceiling $ logBase (fromInteger m) (fromInteger i)
