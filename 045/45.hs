import Data.List (intersect)

import Euler (hexagonal, isPentagonal)

main :: IO ()
main = print . (!! 2) . filter isPentagonal $ hexagonal
