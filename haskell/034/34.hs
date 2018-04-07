import Data.Array.Unboxed
import Data.Char (digitToInt)

main :: IO ()
main = print
     . sum
     . map snd
     . filter (\(a, b) -> a == b)
     . map (tuple $ sum . map (factorial . digitToInt) . show)
     $ [10..1000000]
  where
    tuple f x = (f x, x)
    factorial = (ar !)
    -- cache the values of 1--9 factorial for speed
    ar :: UArray Int Int
    ar = listArray (0, 9) . map (product . enumFromTo 1) $ [0..9]
