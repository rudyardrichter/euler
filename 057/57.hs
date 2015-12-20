import Data.Ratio

import Euler (howMany)

expansions :: [(Integer, Integer)]
expansions = iterate (\(p, q) -> (p + q + q, p + q)) (1, 1)

-- imitation of reduce from GHC.Real for Data.Ratio
reduce :: (Integer, Integer) -> (Integer, Integer)
reduce (p, q) = (p `quot` d, q `quot` d)
  where
    d = gcd p q

main :: IO ()
main = print
     . howMany (\(p, q) -> length (show p) > length (show q))
     . map reduce
     . take 1000
     $ expansions
