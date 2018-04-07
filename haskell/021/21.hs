{-# LANGUAGE Arrows #-}

import Euler ((&.&))
import Primes (divisors)
import Control.Arrow

main :: IO ()
main = print . sum . filter (\n -> n == d (d n) && n /= d n) $ [1..9999]
  where
    d = sum . divisors

-- some (slightly ridiculous) practice with arrows
mainA :: IO ()
mainA = print . sum . filter predicateA $ [1..9999]
  where
    predicateA = pred1 &&& pred2 >>> arr (uncurry (&&))
    pred1 = id &&& d . d >>> arr (uncurry (==))
    pred2 = id &&& d >>> arr (uncurry (/=))
    d = sum . divisors

-- some even more ridiculous practice with arrows
mainDoA :: IO ()
mainDoA = print . sum . filter predicateA $ [1..9999]
  where
    predicateA = proc x -> do
        a <- (unsplit (==) <<< id &&& d . d) -< x
        b <- (unsplit (/=) <<< id &&& d) -< x
        returnA -< a && b
    unsplit = arr . uncurry
    d = sum . divisors
