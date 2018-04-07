main' :: IO ()
main' = print . subtract (sum $ map (^ 2) [1..100]) . (^ 2) . sum $ [1..100]

squareSumTo :: Int -> Int
squareSumTo n = n * (n + 1) * (2 * n + 1) `div` 6

main :: IO ()
main = print . subtract (squareSumTo 100) . (^ 2) $ sum [1..100]
