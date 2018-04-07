import Data.Char (ord, toLower)
import Data.List (sort)

import Euler (splitBy)

main :: IO ()
main = print
     . sum
     . zipWith (*) [1..]
     . map (sum . map (\c -> ord c - ord 'a' + 1))
     . sort
     . map (map toLower)
     . splitBy (== ',')
     . filter (/= '"')
     =<< readFile "022/names.txt"
