module Sort where

import Data.Char
import Data.List

sortme :: [String] -> [String]
sortme = sortBy lower where
  lower x x' = compare (map toLower x) (map toLower x')
