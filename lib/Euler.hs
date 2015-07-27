module Euler where

import Data.Char (digitToInt, intToDigit)
import Data.List (permutations, sort)

(&.&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&.&) f g x = f x && g x

(|.|) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|.|) f g x = f x || g x

-- Convert a number into a list of its digits, e.g. digits 123 = [1, 2, 3].
digits :: Int -> [Int]
digits = map digitToInt . show

-- Test whether a number n is divisible by a number d.
divisible :: (Integral a) => a -> a -> Bool
divisible n d = n `mod` d == 0

-- Filter a list of lists for elements of a specific length
filterLen :: Int -> [[a]] -> [[a]]
filterLen n = filter ((== n) . length)

-- Returns the first element of a list which satisfies the predicate.
first :: (a -> Bool) -> [a] -> a
first f = head . filter f

-- The list of all hexagonal numbers.
hexagonal :: [Int]
hexagonal = scanl1 (+) [1, 5..]

-- Tests whether a number is pentagonal.
isPentagonal :: Int -> Bool
isPentagonal n = isIntegral x && round x `mod` 6 == 5
  where
    x = sqrt $ 24 * (fromIntegral n) + 1
    isIntegral x = fromIntegral (round x) == x

-- Same as maximum, but with a default value to handle [].
maximumDef :: (Ord a) => a -> [a] -> a
maximumDef def xs = if null xs then def else maximum xs

-- Tests whether a list is a palindrome.
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == reverse xs

-- Tests whether a list of Ints is pandigital.
pandigital :: [Int] -> Int -> Bool
pandigital ds = (== ds) . sort . digits

-- The list of all pentagonal numbers.
pentagonals :: [Int]
pentagonals = map (\n -> n * (3 * n - 1) `div` 2) [1..]

-- ! unsafe
-- Read a string as an Int.
readInt :: String -> Int
readInt string = read string :: Int

-- ! unsafe
-- Read a string as an Integer.
readInteger :: String -> Integer
readInteger string = read string :: Integer

-- The reverse of digits; converts a list of digits to an Int.
undigits :: [Int] -> Int
undigits = readInt . map intToDigit
