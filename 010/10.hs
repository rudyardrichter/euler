import Primes (primesTo)

main :: IO ()
main = print . sum $ primesTo 2000000
