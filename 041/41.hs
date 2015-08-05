import Data.List (sort)

import Euler (digits, first)
import Primes (isPrime)

pandigitalPrime :: Int -> Bool
pandigitalPrime n = pandigital && isPrime n
  where
    pandigital = sort ds == take (length ds) [1..9]
    ds = digits n

main :: IO ()
main = print . first pandigitalPrime . reverse $ [1000000..9999999]
