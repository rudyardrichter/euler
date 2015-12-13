import Data.Char (digitToInt)
import Data.List (group, nub, sort)
import Data.Ord (comparing)

import Euler (filterLen)

type Card = (Int, Char)
type Hand = [Card]

scoreHand :: Hand -> [Int]
scoreHand hand
    | ranks == [10..14] && flush = [9]
    | straight && flush          = 8 : ranks
    | kinds == [1, 4]            = 7 : rank 4 ++ rank 1
    | kinds == [2, 3]            = 6 : rank 3 ++ rank 2
    | flush                      = 5 : ranks
    | straight                   = 4 : ranks
    | kinds == [1, 1, 3]         = 3 : rank 3 ++ rank 1
    | kinds == [1, 2, 2]         = 2 : rank 2 ++ rank 1
    | 2 `elem` kinds             = 1 : rank 2 ++ rank 1
    | otherwise                  = 0 : ranks
  where
    ranks    = reverse . sort . map fst $ hand
    suits    = map snd hand
    flush    = all (== head suits) $ tail suits
    straight = head ranks - last ranks == 4 && length (nub ranks) == 5
    kinds    = sort . map length . group $ ranks
    rank n   = reverse . sort . map head . filterLen n . group $ ranks

readCard :: String -> Card
readCard [r, s] = ((readRank r), s)
  where
    readRank r = case r of
        'A' -> 14
        'K' -> 13
        'Q' -> 12
        'J' -> 11
        'T' -> 10
        r   -> digitToInt r

main :: IO ()
main = print
     . length
     . filter ((== GT) . uncurry (comparing scoreHand))
     . map (splitAt 5 . map readCard . words)
     . lines
     =<< readFile "054/poker.txt"
