import Data.List (maximumBy)
import Data.Ord (comparing)

import Euler (divisible, first)
import Primes (primesTo)

discreteLog :: Int -> Integer
discreteLog n = first (\k -> (10 ^ k - 1) `divisible` n') [1..]
  where
    n' = toInteger n

main :: IO ()
main = print
     . maximumBy (comparing discreteLog)
     . dropWhile (<= 5)
     $ primesTo 999
