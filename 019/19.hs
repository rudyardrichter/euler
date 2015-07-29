import Control.Applicative ((<$>), (<*>))

-- Gauss' algorithm for the day of the week of a date
weekday :: Int -- year
        -> Int -- month
        -> Int -- day
        -> Int -- weekday
weekday y m d = (`mod` 7) $ d + floor (2.6 * m' - 0.2) + e
              + e `div` 4 + c `div` 4 - 2 * c
  where
    c  = y' `div` 100
    e  = y' `mod` 100
    m' = fromIntegral . (`mod` 12) $ m - 2
    y' = if m == 1 || m == 2 then y - 1 else y

main :: IO ()
main = print
     . length
     . filter (== 0)
     $ weekday' <$> [1901..2000] <*> [1..12]
  where
    weekday' y m = weekday y m 1
