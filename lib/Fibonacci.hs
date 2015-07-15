module Fibonacci (fib) where

data GL2 = GL2 Integer Integer Integer Integer

mul :: GL2 -> GL2 -> GL2
mul (GL2 a1 b1 c1 d1) (GL2 a2 b2 c2 d2) = GL2 a' b' c' d'
  where
    a' = a1 * a2 + b1 * c2
    b' = a1 * b2 + b1 * d2
    c' = c1 * a2 + d1 * c2
    d' = c1 * b2 + d1 * d2

fastExp :: GL2 -> Int -> GL2
fastExp m e = case e of
    0 -> GL2 1 0 0 1
    1 -> m
    e -> if even e
        then fastExp (mul m m) (div e 2)
        else mul m (fastExp m (pred e))

fib :: Int -> Integer
fib n = let GL2 _ b _ _ = fastExp (GL2 1 1 1 0) n
        in b
