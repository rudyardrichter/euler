import Primes

main :: IO ()
main = print . sum . primesTo $ 2000000
