import Data.Char (digitToInt)

main :: IO ()
main = print . product . map (digitToInt . d) . take 7 . iterate (* 10) $ 1
  where
    d n = (!! (n - 1)) naturals
    -- d = (!!) naturals . pred
    naturals = concatMap show [1..]
