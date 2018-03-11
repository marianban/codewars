module Codewars.Kata.Compare where

import Data.List

comp :: [Integer] -> [Integer] -> Bool
comp as bs = sort bs == as' where
  as' = sort $ map (^2) as
