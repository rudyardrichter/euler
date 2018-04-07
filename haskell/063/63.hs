f :: Int -> [Integer]
f n = takeWhile (< 10 ^ n) . dropWhile (< 10 ^ pred n) $ map (^ n) [1..]

main :: IO ()
main = print . length . concat . takeWhile (not . null) . map f $ [1..]
