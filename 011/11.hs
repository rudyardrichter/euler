import Data.List (transpose)

import Euler (maximumDef, readInt)

maxChunk :: Int -> [Int] -> Int
maxChunk len xs = maximumDef 0
                . map product
                . zipWith (\i -> take len . drop i) [0..length xs - len]
                $ repeat xs

hz :: Int -> [[Int]] -> Int
hz n = maximum . map (maxChunk n)

vt :: Int -> [[Int]] -> Int
vt n = hz n . transpose

ld :: Int -> [[Int]] -> Int
ld n = vt n . filter (not . null) . zipWith drop [0..]

rd :: Int -> [[Int]] -> Int
rd n = ld n . map reverse

main :: IO ()
main = print
     . maximum
     . (\grid -> [hz 4 grid, vt 4 grid, ld 4 grid, rd 4 grid])
     . map (map readInt . words)
     . lines
     =<< readFile "011/grid.txt"
