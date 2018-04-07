import Combinatorics (pairs)
import Euler         (readInt)
import Primes        (isPrime, primesTo)

primeConcat :: Int -> Int -> Bool
primeConcat x y = forward && backward
  where
    forward  = isPrime . readInt $ show x ++ show y
    backward = isPrime . readInt $ show y ++ show x

concatsWith :: Int -> [Int] -> [Int]
concatsWith n = filter (primeConcat n)

main :: IO ()
main = print . sum . head $ [ [a, b, c, d, e]
                            | a <- ps
                            , let bs = validFrom a ps
                            , b <- bs
                            , let cs = validFrom b bs
                            , c <- cs
                            , let ds = validFrom c cs
                            , d <- ds
                            , let es = validFrom d ds
                            , e <- es
                            ]
  where
    ps = drop 5 $ primesTo 10000
    validFrom x = concatsWith x . dropWhile (<= x)
