import Control.Applicative ((<$>), (<*>))

import Euler (palindrome)

main :: IO ()
main = print . maximum . filter (palindrome . show)
     $ (*) <$> [100..999] <*> [100..999]
