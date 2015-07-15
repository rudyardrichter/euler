-- module Primes
-- Functions for primality testing, prime generation, factorization, and
-- (tangentially) modular arithmetic.

module Primes (
    divisors,
    factorize,
    isPrime,
    millerRabin,
    modExp,
    primePowers,
    primesArray,
    primesTo,
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.List (group)
import System.Random

-----------------------------------------------------------------------------
-- Primality testing

isPrimeMR :: Integer -> IO Bool
isPrimeMR = millerRabin 50

millerRabin :: Int -> Integer -> IO Bool
millerRabin k n = if even n
    then return (n == 2)
    else witnesses k n >>= return . or . map (test n)

witnesses :: Int -> Integer -> IO [Integer]
witnesses k n = newStdGen >>= return . take k . randomRs (2, n - 2)

test :: Integer -> Integer -> Bool
test n w = w' == 1 || squareTest w'
  where
    w' = modExp w d n
    squareTest = any (== n - 1) . take s . iterate modSquare
    modSquare = ((`mod` n) . (^ 2))
    -- find s and d : (n - 1) == d * 2^s
    s = until (\power -> testBit (n - 1) power) succ 0
    d = (n - 1) `div` (2 ^ s)

-----------------------------------------------------------------------------
-- Modular functions

-- calculate the inverse of n (mod m)
modInv :: (Integral a) => a -> a -> a
modInv n m = let (r, x, _) = gcde n m in
    if r == 1
        then x `mod` m
        else -1

-- extended Euclidean algorithm
gcde :: (Integral a) => a -> a -> (a, a, a)
gcde a b = (sign * r, sign * x, sign * y)
  where
    sign = signum r
    (r, x, y) = loop (a, 1, 0) (b, 0, 1)
    loop (r1, x1, y1) (r2, x2, y2)
        | r2 == 0   = (r1, x1, y1)
        | otherwise = loop (r2, x2, y2) (r, x1 - q * x2, y1 - q * y2)
      where
        q = r1 `div` r2
        r = r1 `mod` r2

-- compute b ^ e (mod m)
modExp :: (Integral a, Integral b) => a -> b -> a -> a
modExp b e m = loop b e 1
  where
    loop x y n
        | y == 0 = n
        | mod y 2 == 0 = loop x' y' n
        | otherwise = loop x' y' (x * n `mod` m)
      where
        x' = x * x `mod` m
        y' = y `div` 2

-----------------------------------------------------------------------------
-- Primes generation, factorization

-- from the Haskell wiki page on primes
primesArray :: Int -> UArray Int Bool
primesArray bound = runSTUArray $ do
    let m = (pred bound) `div` 2
    let r = (`div` 2) . floor . sqrt $ ((fromIntegral bound + 1) :: Double)
    sieve <- newArray (1, m) True
    forM_ [1..r] $ \n -> do
        isPrime <- readArray sieve n
        when isPrime $ do
            forM_ [2 * n * (n + 1), 2 * n * (n + 2) + 1..m] $ \c -> do
                writeArray sieve c False
    return sieve

primesTo :: Int -> [Int]
primesTo = (2:) . map (succ . (* 2) . fst) . filter snd . assocs . primesArray

isPrime :: Int -> Bool
isPrime n
    | n == 1    = False
    | even n    = n == 2
    | otherwise = not
                . any divisor
                . primesTo
                . floor
                . sqrt
                . fromIntegral
                $ n
  where
    divisor x = n `mod` x == 0

factorize :: (Integral a) => a -> [a]
factorize = loop (2:[3,5..])
  where
    loop (d:ds) n
        | d > n     = []
        | r == 0    = d : loop ds q
        | otherwise = loop ds n
      where
        (q, r) = quotRem n d

primePowers :: (Integral a) => a -> [(a, Int)]
primePowers n = [(head ds, length ds) | ds <- group $ loop (2:[3,5..]) n]
  where
    loop (d:ds) n
        | d * d > n = [n]
        | r == 0    = d : loop (d:ds) q
        | otherwise = loop ds n
      where
        (q, r) = quotRem n d

divisors :: (Integral a) => a -> [a]
divisors = map product
         . init
         . sequence
         . map (\(p, k) -> take (succ k) $ iterate (* p) 1)
         . primePowers