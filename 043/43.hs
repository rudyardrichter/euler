import Data.Char (intToDigit)
import Data.List (permutations, sort)

import Euler (divisible, readInt)
import Primes (primesTo)

chunk :: Int -> Int -> Int
chunk n = read . take 3 . drop n . show

interesting :: Int -> Bool
interesting n = and . zipWith divBy primes . map (flip chunk n) $ [1..7]
  where
    primes = primesTo 17
    divBy = flip divisible

main :: IO ()
main = print . sum . filter interesting $ pandigitals
  where
    pandigitals = map (readInt . map intToDigit) . permutations $ [0..9]
