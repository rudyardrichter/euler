import Control.Applicative ((<$>), (<*>))
import Data.Char (digitToInt)

main :: IO ()
main = print
     . maximum
     . map (sum . map digitToInt . show)
     $ (^) <$> [1..99] <*> [1..99]

-- when (map digitToInt . show) is replaced with digits (from Euler), main
-- returns "56: Char.digitToInt: not a digit '-'" ... ??
