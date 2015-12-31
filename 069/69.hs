import Primes (primes)

main :: IO ()
main = print . last . takeWhile (< 1000000) . scanl1 (*) $ primes
