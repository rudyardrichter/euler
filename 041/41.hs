import Data.List (permutations)

import Euler (undigits)
import Primes (isPrime)

main :: IO ()
main = print
     . maximum
     . filter isPrime
     . map undigits
     . permutations
     $ [1..7]
