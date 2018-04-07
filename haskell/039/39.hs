import Data.List (maximumBy)
import Data.Ord (comparing)

triangles :: Int -> [(Int, Int, Int)]
triangles p = [ (a, b, c)
              | a <- [1..p `div` 3]
              , b <- [a..p - a]
              , c <- [p - b - a]
              , a * a + b * b == c * c
              ]

main :: IO ()
main = print . maximumBy (comparing $ length . triangles) $ [1..1000]
