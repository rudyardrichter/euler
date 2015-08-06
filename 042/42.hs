import Data.Char

import Euler (splitBy, triangulars)

main :: IO ()
main = print
     . length
     . filter ((`ordElem` triangulars) . sum . map (charToNum . toLower))
     . splitBy (== ',')
     . filter (/= '"')
     =<< readFile "042/words.txt"
  where
    charToNum c = ord c - ord 'a' + 1
    ordElem x ys = x `elem` takeWhile (<= x) ys
