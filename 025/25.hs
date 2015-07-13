import Fibonacci

main :: IO ()
main = print
     . head
     . dropWhile ((< 1000) . length . show . fib)
     $ [1..]
