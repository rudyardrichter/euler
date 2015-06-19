main :: IO ()
main = print . sum . filter multiple3or5 $ [1..999]
  where
    multiple3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0
