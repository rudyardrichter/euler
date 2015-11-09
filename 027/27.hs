import Control.Applicative ((<$>), (<*>))
import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.Set (Set)
import qualified Data.Set as Set

import Primes (primesTo)

main :: IO ()
main = print
     . result
     . snd
     . maximumBy (comparing fst)
     . map (tuple $ length . takeWhile (`Set.member` primes) . evalQF)
     $ (,) <$> [-1000..1000] <*> [-1000..1000]
  where
    result (b, c) = b * c
    tuple f x = (f x, x)
    primes = Set.fromDistinctAscList $ primesTo 1000
    evalQF (b, c) = map (\x -> abs $ x * x + b * x + c) [1..]
