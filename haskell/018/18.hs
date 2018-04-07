maxRoute :: [[Int]] -> Int
maxRoute triangle = recurse triangle 0

recurse :: [[Int]] -> Int -> Int
recurse [] base = base
recurse triangle base = (+ base)
                      . maximum
                      . zipWith recurseSubTriangle [0..]
                      $ head triangle
  where
    recurseSubTriangle :: Int -> Int -> Int
    recurseSubTriangle i base = recurse (subTriangle (tail triangle) i) base

subTriangle :: [[Int]] -> Int -> [[Int]]
subTriangle tri i = loop tri i 2
  where
    loop [] _ _ = []
    loop (row:rows) i j = ((take j . drop i) row) : loop rows i (succ j)

main :: IO ()
main = print
     . maxRoute
     . map (map (\x -> read x :: Int) . words)
     . lines
     =<< readFile "018/triangle.txt"
