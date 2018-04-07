import Control.Arrow

ring :: Int -> Int
ring n = 4 * (2 * n + 1) ^ 2 - 12 * n

ringA :: Int -> Int
ringA = ((* 4) . (^ 2) . succ . (* 2)) &&& (* 12) >>> arr (uncurry (-))

main :: IO ()
main = print . succ . sum . map ring $ [1..500]

mainA :: IO ()
mainA = print . succ . sum . map ringA $ [1..500]
