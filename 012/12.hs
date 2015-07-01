import Primes

main :: IO ()
main = print
     . head
     . filter ((> 500) . length . divisors)
     . scanl1 (+)
     $ [1..]
