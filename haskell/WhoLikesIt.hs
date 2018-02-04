module Likes where

likes :: [String] -> String
likes [] = "no one likes this"
likes [x] = x ++ " likes this"
likes [x, x'] = x ++ " and " ++ x' ++ " like this"
likes [x, x', x''] = x ++ ", " ++ x' ++ " and " ++ x'' ++ " like this"
likes (x:x':xs) = x ++ ", " ++ x' ++ " and " ++ show (length xs) ++ " others like this"
