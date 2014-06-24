module Math.Polynomial.Check
       ( checkQuotsRem ) where

import GHC.TypeLits (SingI)
import Math.Polynomial.Imports
  (DegreeOrder, Polynomial, PolyQuots, (/.))

checkQuotsRem' :: (Fractional k, Ord k, SingI n, DegreeOrder o)
               => Polynomial o k n
               -> PolyQuots o k n
               -> Polynomial o k n
               -> Bool
checkQuotsRem' f qs r =
  f == foldr (+) 0 [ d * q | (d, q) <- qs] + r

checkQuotsRem :: (Fractional k, Ord k, SingI n, DegreeOrder o)
              => Polynomial o k n
              -> [Polynomial o k n]
              -> Bool
checkQuotsRem f ds = checkQuotsRem' f qs r
  where (qs, r) = f /. ds
