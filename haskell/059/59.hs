import Control.Applicative ((<$>), (<*>))
import Data.Bits (xor)
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Euler ((|.|), first, readInt, splitBy)

keys :: [[Int]]
keys = map list $ (,,) <$> chars <*> chars <*> chars
  where
    chars = map ord ['a'..'z']
    list (a, b, c) = [a, b, c]

english :: String -> Bool
english = all (isAlphaNum |.| isSpace |.| isPunctuation)

main :: IO ()
main = do
    file <- readFile "059/cipher.txt"
    let cipher = map readInt $ splitBy (== ',') file
        decrypt key = map chr $ zipWith xor (cycle key) cipher
        attempts = map decrypt keys
    print . sum . map ord $ first english attempts
