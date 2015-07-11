import Data.Char (ord, toLower)
import Data.List (sort)

main :: IO ()
main = print
     . sum
     . zipWith (*) [1..]
     . map (sum . map (\c -> ord c - ord 'a' + 1))
     . sort
     . map (map toLower)
     . wordsBy (== ',')
     . filter (/= '"')
     =<< readFile "022/names.txt"

wordsBy p str = case dropWhile p str of
    "" -> []
    str' -> w : wordsBy p str''
      where
        (w, str'') = break p str'
