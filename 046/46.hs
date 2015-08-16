import Euler ((&.&))
import Primes (isPrime)

valid :: Int -> Bool
valid n = any isPrime
        . map (n -)
        . takeWhile (< n)
        $ twiceSquares
  where
    twiceSquares = map (\i -> 2 * i * i) [1..]

main :: IO ()
main = print
     . head
     . filter ((not . valid) &.& (not . isPrime))
     $ [9, 11..]
