module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference = foldl filterInput where
  filterInput xs y = filter (/=y) xs
