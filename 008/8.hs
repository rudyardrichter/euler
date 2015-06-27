import Data.Char (digitToInt)

main :: IO ()
main = print
     . maximum
     . map product
     . zipWith (\i -> take 13 . drop i) [0..987]
     . repeat
     . map digitToInt
     . filter (/= '\n')
     =<< readFile "008/number.txt"
