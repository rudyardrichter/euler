import Data.List (nub, sort)

import Euler (digits)

main :: IO ()
main = print . sum . nub $ [ p
                           | a <- [2..50]
                           , b <- [a..2000]
                           , p <- [a * b]
                           , sort (digits a ++ digits b ++ digits p) == [1..9]
                           ]
