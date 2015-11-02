import Data.Array.Unboxed

import Primes (divisors)

sumOfAbundantsTo :: Int -> Int -> Bool
sumOfAbundantsTo bound n = any (abundants !)
                         . map (n -)
                         . takeWhile (<= n `div` 2)
                         . filter (abundants !)
                         $ [1..bound]
  where
    abundants :: UArray Int Bool
    abundants = listArray (1, bound) $ map isAbundant [1..bound]
    isAbundant n = sum (divisors n) > n

main :: IO ()
main = print . sum . filter (not . sumOfAbundantsTo bound) $ [1..bound]
  where
    bound = 28123
