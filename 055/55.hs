import Euler (howMany, palindrome, readInteger)

reverseAdd :: Integer -> Integer
reverseAdd n = (+ n) . readInteger . reverse . show $ n

lychrel :: Integer -> Bool
lychrel = loop 49 . reverseAdd
  where
    loop count n
        | count == 0 = True
        | palindrome (show n) = False
        | otherwise = loop (pred count) (reverseAdd n)

main :: IO ()
main = print $ howMany lychrel [1..9999]

-- This already runs rather quickly (~ 150 ms), but the speed might also be
-- improved by using an array to memoize the reverseAdd iterations
