import Euler ((&.&))
import Primes (isPrime)

truncateLeft :: Int -> Int
truncateLeft n = n `mod` 10 ^ (pred . length $ show n)

truncateRight :: Int -> Int
truncateRight = (`div` 10)

truncationsLeft :: Int -> [Int]
truncationsLeft = takeWhile (> 0) . iterate truncateLeft

truncationsRight :: Int -> [Int]
truncationsRight = takeWhile (> 0) . iterate truncateRight

truncations :: Int -> [Int]
truncations n = tail (truncationsLeft n) ++ truncationsRight n

main :: IO ()
main = print
     . sum
     . take 11
     . filter ((all isPrime . truncations) &.& isPrime)
     $ [11, 13..]
