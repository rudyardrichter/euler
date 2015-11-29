import Euler
import Primes (isPrime, primesTo)

descSubSequences :: [a] -> [[a]]
descSubSequences xs = loop 1 0 xs
  where
    loop i j xs
        | i == len  = []
        | j == i    = drop i xs : loop (succ i) 0 xs
        | otherwise = take (len - i) (drop j xs) : loop i (succ j) xs
    len = length xs

main :: IO ()
main = print
     . first isPrime
     . filter (< 1000000)
     . map sum
     . descSubSequences
     . primesTo
     $ 4000

-- consecutive primes above [2..4000] sum to more than 1000000
