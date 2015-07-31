import Data.Char (intToDigit)
import Numeric (showIntAtBase)

import Euler (palindrome)

main :: IO ()
main = print
     . sum
     . filter (palindrome . showBinary)
     . filter (palindrome . show)
     $ [1..999999]
  where
    showBinary n = showIntAtBase 2 intToDigit n ""
