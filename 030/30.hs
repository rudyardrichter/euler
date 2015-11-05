import Control.Arrow
import Data.Char (digitToInt)

import Euler (digits)

main :: IO ()
main = print . sum . filter f $ [10..200000]
  where
    f x = fifthPowerSum x == x
    fifthPowerSum = sum . map (^ 5) . digits

mainA :: IO ()
mainA = print
      . sum
      . filter (sum . map (^ 5) . digits &&& id >>> arr (uncurry (==)))
      $ [10..200000]
