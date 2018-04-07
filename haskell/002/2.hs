import Fibonacci

main :: IO ()
main = print . sum . filter even . takeWhile (< 4000000) . map fib $ [0..]
