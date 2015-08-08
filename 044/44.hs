import Euler (isPentagonal, pentagonals)

main :: IO ()
main = print $ head [ p2 - p1
                    | p2 <- pentagonals
                    , p1 <- takeWhile (< p2) pentagonals
                    , isPentagonal $ p1 + p2
                    , isPentagonal $ p2 - p1
                    ]
