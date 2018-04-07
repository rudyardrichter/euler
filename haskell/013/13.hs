import Euler (readInteger)

main :: IO ()
main = putStrLn
     . take 10
     . show
     . sum
     . map readInteger
     . lines
     =<< readFile "013/numbers.txt"
