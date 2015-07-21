countChange :: [Int] -> Int -> Int
countChange coins x = count x (length coins)
  where
    count x coin
        | x == 0    = 1
        | x < 0     = 0
        | coin <= 0 = 0
        | otherwise = memoize x (coin - 1)
                    + memoize (x - (coins !! (coin - 1))) coin
    memoize x coin
        | x == 0    = 1
        | x < 0     = 0
        | coin <= 0 = 0
        | otherwise = memos !! x !! coin
    memos = memoize2 count
    memoize2 f = map (\x -> map (f x) [0..]) [0..]

main :: IO ()
main = print $ countChange [1, 2, 5, 10, 20, 50, 100, 200] 200
