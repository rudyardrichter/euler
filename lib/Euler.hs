module Euler where

import Data.Char (digitToInt, intToDigit)
import Data.List (permutations, sort)

(&.&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&.&) f g x = f x && g x

(|.|) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|.|) f g x = f x || g x

digits :: Int -> [Int]
digits = map digitToInt . show

divisible :: (Integral a) => a -> a -> Bool
divisible n d = n `mod` d == 0

filterLen :: Int -> [[a]] -> [[a]]
filterLen n = filter ((== n) . length)

first :: (a -> Bool) -> [a] -> a
first f = head . filter f

intPermutations :: Int -> [Int]
intPermutations = map readInt . permutations . show

isPentagonal :: Int -> Bool
isPentagonal n = isIntegral x && round x `mod` 6 == 5
  where
    x = sqrt $ 24 * (fromIntegral n) + 1
    isIntegral x = fromIntegral (round x) == x

maximumDef :: (Ord a) => a -> [a] -> a
maximumDef def xs = if null xs then def else maximum xs

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == reverse xs

pandigital :: [Int] -> Int -> Bool
pandigital ds = (== ds) . sort . digits

pentagonals :: [Int]
pentagonals = map (\n -> n * (3 * n - 1) `div` 2) [1..]

-- ! unsafe
readInt :: String -> Int
readInt string = read string :: Int

readInteger :: String -> Integer
readInteger string = read string :: Integer

undigits :: [Int] -> Int
undigits = readInt . map intToDigit
