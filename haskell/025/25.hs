import Euler (first)
import Fibonacci

main :: IO ()
main = print $ first ((>= 1000) . length . show . fib) [1..]
