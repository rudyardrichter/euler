import Data.List (foldl1')

collatzLen :: Int -> Int
collatzLen = loop
  where
    loop 1 = 0
    loop n = succ . loop . step $ n
    step n
        | r == 0    = q
        | otherwise = succ $ 3 * n
      where
        (q, r) = quotRem n 2

main :: IO ()
main = print
     . foldl1' (\acc x -> if collatzLen x >= collatzLen acc then x else acc)
     $ [1..999999]
