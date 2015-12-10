import Data.List (sort)

import Euler (first)

main :: IO ()
main = print . first valid $ [1..]
  where
    valid n = all (sameDigits n) . tail . take 6 . iterate (+ n) $ n
    sameDigits a b = f a == f b
    f = sort . show
