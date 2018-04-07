import Data.List (nub)

import Euler (first)
import Primes (primePowers)

distinctFactors :: Int -> Int -> Bool
distinctFactors k n = k == length factors
  where
    factors = nub $ concatMap (map combine . primePowers) [n..n + 3]
    combine (a, b) = a * b

main :: IO ()
main = print $ first (distinctFactors 16) [1..]
