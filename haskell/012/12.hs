import Euler (first, triangulars)
import Primes (divisors)

main :: IO ()
main = print $ first ((> 500) . length . divisors) triangulars
