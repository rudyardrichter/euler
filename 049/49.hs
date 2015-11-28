import Control.Applicative ((<$>), (<*>))
import Data.List (permutations)

import Euler (first, readInt)
import Primes (isPrime)

primePermutations :: Int -> [Int]
primePermutations = filter isPrime . map readInt . permutations . show

main :: IO ()
main = putStrLn . format . head $ [ (a, b, c)
                                  | a <- [1000..9999]
                                  , b <- primePermutations a
                                  , c <- primePermutations a
                                  , a < b
                                  , b < c
                                  , a /= 1487
                                  , isPrime a
                                  , b + b == a + c
                                  ]
  where
    format (a, b, c) = concatMap show [a, b, c]
