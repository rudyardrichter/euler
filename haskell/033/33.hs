import Control.Applicative ((<$>), (<*>))
import Data.Ratio

-- there is definitely a shorter way of doing this

data Frac = Frac Int Int Int Int deriving (Eq, Show)

fracToRatio :: Frac -> Ratio Int
fracToRatio (Frac a b c d) = (10 * a + b) % (10 * c + d)

cancel :: Frac -> Frac
cancel f@(Frac a b c d)
    | b == d && a /= 0 && b /= 0 && c /= 0 = Frac 0 a 0 c
    | b == c && a /= 0 && d /= 0 = Frac 0 a 0 d
    | a == d && b /= 0 && c /= 0 = Frac 0 b 0 c
    | a == c && b /= 0 && d /= 0 = Frac 0 b 0 d
    | otherwise = f

curious :: Frac -> Bool
curious frac = cancels && ratio < (1 % 1)
  where
    cancels = ratio == fracToRatio (cancel frac) && frac /= cancel frac
    ratio = fracToRatio frac

main :: IO ()
main = print
     . denominator
     . product
     . map fracToRatio
     . filter curious
     . filter nonzero
     $ Frac <$> [0..9] <*> [0..9] <*> [0..9] <*> [0..9]
  where
    nonzero (Frac a b c d) = (a /= 0 || b /= 0) && (c /= 0 || d /= 0)
