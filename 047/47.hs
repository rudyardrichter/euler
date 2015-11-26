import Data.List (nub)

import Euler (first)
import Primes (primePowers)

distinctFactors :: Int -> Bool
distinctFactors n = length factors == 16
  where
    factors = nub $ concatMap (map combine . primePowers) [n..n + 3]
    combine (a, b) = a * b

main :: IO ()
main = print $ first distinctFactors [1..]
