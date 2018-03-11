module TheShellGame where

-- Given the starting index of the ball, and the sequence of swaps,
-- return the final position of the ball.
findTheBall :: Int -> [(Int, Int)] -> Int
findTheBall pos [] = pos
findTheBall pos ((fromPos, toPos):poss)
 | pos == fromPos = findTheBall toPos poss
 | pos == toPos = findTheBall fromPos poss
 | otherwise = findTheBall pos poss
