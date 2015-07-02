main :: IO ()
main = putStrLn
     . take 10
     . show
     . sum
     . map (\x -> read x :: Integer)
     . lines
     =<< readFile "013/numbers.txt"
