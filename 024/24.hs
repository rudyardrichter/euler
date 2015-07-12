import Data.List (permutations, sort)

-- not quick, but easy
main :: IO ()
main = putStrLn
     . foldl1 (++)
     . map show
     . (!! 999999)
     . sort
     . permutations
     $ [0..9]
