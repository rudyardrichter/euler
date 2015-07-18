ring :: Int -> Int
ring n = 4 * (2 * n + 1) ^ 2 - 12 * n

main :: IO ()
main = print . succ . sum $ map ring [1..500]
