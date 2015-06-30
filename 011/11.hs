import Data.List (transpose)

maxChunk :: Int -> [Int] -> Int
maxChunk len xs = maximum'
                . map product
                . zipWith (\i -> take len . drop i) [0..length xs - len]
                $ repeat xs
  where
    maximum' xs = if null xs then 0 else maximum xs

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
     . map (map (\w -> read w :: Int) . words)
     . lines
     =<< readFile "011/grid.txt"
