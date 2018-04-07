import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

step :: Int -> Int
step n
    | r == 0    = q
    | otherwise = succ $ 3 * n
  where
    (q, r) = quotRem n 2

collatzLengthsTo :: Int -> Array Int Int
collatzLengthsTo bound = ar
  where
    ar = listArray (1, bound) $ 0 : [collatz x | x <- [2..bound]]
    collatz x
        | x' <= bound   = succ $ ar ! x'
        | otherwise     = succ $ collatz x'
      where
        x' = step x

main :: IO ()
main = print
     . fst
     . maximumBy (comparing snd)
     . assocs
     . collatzLengthsTo
     $ 1000000
