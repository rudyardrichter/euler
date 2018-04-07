import Euler ((&.&), digits, pandigital, undigits)
import Primes (isPrime)

rotations :: [a] -> [[a]]
rotations x = take (length x) $ iterate rotate x
  where
    rotate [] = []
    rotate (x:xs) = xs ++ [x]

main :: IO ()
main = print
     . (+ 4)
     . length
     . filter ((rightDigits &.& primeRotations) . digits)
     $ [11, 13..999999]
  where
    rightDigits = all (`elem` [1, 3, 7, 9])
    primeRotations = all isPrime . map undigits . rotations
