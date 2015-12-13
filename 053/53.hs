import Combinatorics (choose)

main :: IO ()
main = print . length $ [ n `choose` r
                        | n <- [1..100]
                        , r <- [1..n]
                        , n `choose` r > 1000000
                        ]
