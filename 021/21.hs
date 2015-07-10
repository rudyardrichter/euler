import Primes

main :: IO ()
main = print
     . sum
     . filter (\n -> n == d (d n) && n /= d n)
     $ [1..9999]
  where
    d = sum . divisors
