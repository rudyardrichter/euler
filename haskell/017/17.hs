onesToLength :: Int -> Int
onesToLength ones = case ones of
    1 -> 3
    2 -> 3
    3 -> 5
    4 -> 4
    5 -> 4
    6 -> 3
    7 -> 5
    8 -> 5
    9 -> 4
    _ -> 0

twosToLength :: Int -> Int
twosToLength twos = case twos of
    2 -> 6
    3 -> 6
    4 -> 5
    5 -> 5
    6 -> 5
    7 -> 7
    8 -> 6
    9 -> 6
    _ -> 0

tensToLength :: Int -> Int -> Int
tensToLength ones twos
    | twos == 1 = case ones of
        0 -> 3
        1 -> 6
        2 -> 6
        3 -> 8
        4 -> 8
        5 -> 7
        6 -> 7
        7 -> 9
        8 -> 8
        9 -> 8
        _ -> 0
    | otherwise = onesToLength ones + twosToLength twos

threesToLength :: Int -> Int -> Int
threesToLength threes tens
    | threes == 0 = 0
    | tens == 0   = onesToLength threes + 7
    | otherwise   = onesToLength threes + 10

numToLength :: Int -> Int
numToLength x = tens + threesToLength threes tens
  where
    ones   = x `mod` 10
    twos   = (x `mod` 100) `div` 10
    threes = x `div` 100
    tens   = tensToLength ones twos

main :: IO ()
main = print . (+ 11) . sum . map numToLength $ [1..999]
