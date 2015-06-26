import Primes

main :: IO ()
main = print . (!! 10000) . primesTo $ 125000
