module Combinatorics where

choose :: (Integral a) => a -> a -> a
choose n k
    | n == 0    = 0
    | k == 0    = 1
    | otherwise = n * choose (pred n) (pred k) `div` k

pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs
