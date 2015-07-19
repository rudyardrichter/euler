import Control.Applicative ((<$>), (<*>))
import Data.List (nub)

main :: IO ()
main = print . length . nub $ (^) <$> [2..100] <*> [2..100]
