import Data.List (permutations, sort)

-- not quick, but easy
main :: IO ()
main = putStrLn . concatMap show . (!! 999999) . sort . permutations $ [0..9]
