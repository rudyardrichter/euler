-- http://arxiv.org/pdf/math/0601660.pdf

import Euler (digits)

memoize :: (Int -> a) -> Int -> a
memoize f = (map f [0..] !!)

reduce :: (Integral a) => (a, a) -> (a, a)
reduce (p, q) = (p `quot` d, q `quot` d)
  where
    d = gcd p q

eConvergent :: Int -> (Integer, Integer)
eConvergent = reduce . memoize f
  where
    f n = case n of
        0 -> (2, 1)
        1 -> (3, 1)
        n -> (e n * p (n - 1) + p (n - 2), e n * q (n - 1) + q (n - 2))
    p = fst . eConvergent
    q = snd . eConvergent
    e n = (!! n) $ 2 : 1 : loop 1
      where
        loop n = 2 * n : 1 : 1 : loop (succ n)

main :: IO ()
main = print . sum . digits . fst . eConvergent . pred $ 100
