import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = print . maximum . filter palindrome
     $ (*) <$> [100..999] <*> [100..999]
  where
    palindrome x = show x == reverse (show x)
