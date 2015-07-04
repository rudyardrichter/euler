choose :: Integer -> Integer -> Integer
choose n k
    | n == 0    = 0
    | k == 0    = 1
    | otherwise = n * choose (pred n) (pred k) `div` k

-- combinatorial method: (20 + 20) choose 20
main = print $ choose 40 20
