module Codewars.G964.Longestconsec where

import Data.List

longestConsec :: [String] -> Int -> String
longestConsec strarr k
 | k == 0 = ""
 | length strarr < k = ""
 | null strarr = ""
 | otherwise = let array = getConsec strarr in
 snd $ maximumBy (\(x, _) (y, _) -> compare x y) $ reverse array
 where
  getConsec [] = []
  getConsec (x:ys) =
    let str = (x ++ concat(take (k-1) ys))
    in (length str, str) : getConsec ys
