import Control.Arrow

main :: IO ()
main = print . (`mod` 10000000000) . sum . map selfPower $ [1..1000]
  where
    selfPower n = n ^ n
    selfPower' = id &&& id >>> arr (uncurry (^))
