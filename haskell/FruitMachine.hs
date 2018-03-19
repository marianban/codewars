module Haskell.SylarDoom.FruitMachine where

import Data.List
import Data.Maybe

scores :: [(String, Int)]
scores = zip ["Wild","Star","Bell","Shell","Seven","Cherry","Bar","King","Queen","Jack"] (reverse [1..10])

scoreFor :: String -> Int
scoreFor reel = snd $ fromJust $find (\(r, _) -> r == reel) scores

score :: [Int] -> Int
score [] = 0
score reels
   | length reelGroup == 3 = 0
   | length reelGroup == 1 = head (head reelGroup) * 10
   | length reelGroup == 2 = case head (head reelGroup) of
       10 -> head (last reelGroup) * 2
       _ -> head (last reelGroup)
   | otherwise = error "Invalid reels"
     where reelGroup = sortBy (\xs ys -> (compare (length xs) (length ys))) (group $ sort reels)


fruit :: [[String]] -> [Int] -> Int
fruit reels spins = score $ map (\(reel, spin) -> scoreFor(reel !! spin)) (zip reels spins)
