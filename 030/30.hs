import Data.Char (digitToInt)

main :: IO ()
main = print . sum . filter f $ [10..200000]
  where
    f x = fifthPowerSum x == x
    fifthPowerSum = sum . map ((^ 5) . digitToInt) . show
