module Primes (
    isPrime,
    factorize,
    millerRabin,
    primes,
    primesTo,
    rndPrime,
    rndPrimes
    ) where

import Control.Monad
import Data.Bits
import System.Random

-----------------------------------------------------------------------------
-- Primality testing

isPrime :: Integer -> IO Bool
isPrime = millerRabin 50

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
    modSquare = ((`mod` n) . (^2))
    -- find s and d : (n - 1) == d * 2^s
    s = until (\power -> testBit (n - 1) power) succ 0
    d = (n - 1) `div` (2^s)

-----------------------------------------------------------------------------
-- Random prime generation
-- (originally for use with RSA)

rndPrime :: Int -> IO Integer
rndPrime bits = doMUntil isPrime candidate
  where
    -- (.|. 1) makes the candidate odd if it wasn't already
    candidate = fmap (.|. 1) $ randomRIO (lower, upper)
    lower     = 2 ^ (bits - 1)
    upper     = 2 ^ bits - 1

rndPrimes :: Int -> IO (Integer, Integer)
rndPrimes bits = do
    p <- rndPrime bits
    q <- doUntil (/= p) $ rndPrime bits
    return (p, q)

doUntil :: (Monad m) => (a -> Bool) -> m a -> m a
doUntil p mx = loop
  where
    loop = do
    x <- mx
    if p x
        then return x
        else loop

doMUntil :: (Monad m) => (a -> m Bool) -> m a -> m a
doMUntil mp mx = loop
  where
    loop = do
        x <- mx
        p <- mp x
        if p
            then return x
            else loop

-----------------------------------------------------------------------------
-- Modular functions

-- calculate the inverse of n (mod m)
modInv :: Integer -> Integer -> Integer
modInv n m = let (r,x,_) = gcde n m in
    if r == 1
        then x `mod` m
        else error $ (show n) ++ " has no inverse mod " ++ (show m)

-- extended Euclidean algorithm
gcde :: Integer -> Integer -> (Integer, Integer, Integer)
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
-- Prime generation, factorization

primes :: [Integer]
primes = 2 : sieve [3,5..]

primesTo :: Integer -> [Integer]
primesTo bound = 2 : sieve [3,5..bound]

-- TODO: implement with Data.Array
sieve :: [Integer] -> [Integer]
sieve (p:ns)
    | null ns   = [p]
    | otherwise = p : sieve (ns `minus` [p * p, p * p + 2 * p..])

minus :: (Ord a) => [a] -> [a] -> [a]
minus l1@(x:xs) l2@(y:ys)
    | null l1   = []
    | null l2   = l1
    | x > y     = minus l1 ys
    | x < y     = x : minus xs l2
    | otherwise = minus xs l2

factorize :: Integer -> [Integer]
factorize = loop primes
  where
    loop (p:ps) x
        | p > x          = []
        | x `mod` p == 0 = p : loop ps (x `div` p)
        | otherwise      = loop ps x
