import Primes

main :: IO ()
main = print . maximum . factorize $ 600851475143
