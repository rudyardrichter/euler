import Data.List (sort)

import Euler (pandigital)

concatProductTo :: Int -> Int -> Int
concatProductTo n x = read $ loop [1..n] x
  where
    loop :: [Int] -> Int -> String
    loop [] n = []
    loop (x:xs) n = show (x * n) ++ loop xs n

main :: IO ()
main = print
     . maximum
     . map last
     . filter (not . null)
     . map products
     $ [2..9]
  where
    products n = filter (pandigital [1..9])
               . takeWhile (< 1000000000)
               . map (concatProductTo n)
               $ [1..]
