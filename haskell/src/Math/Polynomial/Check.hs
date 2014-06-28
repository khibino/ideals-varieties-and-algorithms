module Math.Polynomial.Check
       ( checkQuotsRem, checks ) where

import GHC.TypeLits (SingI)
import Math.Polynomial.Imports
  (DegreeOrder, Polynomial, PolyQuot (..), PolyQuotsRem (..), (/.))

checkQuotsRem' :: (Fractional k, Ord k, SingI n, DegreeOrder o)
               => Polynomial o k n
               -> [PolyQuot o k n]
               -> Polynomial o k n
               -> Bool
checkQuotsRem' f qs r =
  f == foldr (+) 0 [ q * d | PolyQuot q d <- qs] + r

checkQuotsRem :: (Fractional k, Ord k, SingI n, DegreeOrder o)
              => Polynomial o k n
              -> [Polynomial o k n]
              -> Bool
checkQuotsRem f ds = checkQuotsRem' f qs r
  where PolyQuotsRem qs r = f /. ds

checks :: (Fractional k, Ord k, SingI n, DegreeOrder o)
       => [Polynomial o k n]
       -> [[Polynomial o k n]]
       -> [(Polynomial o k n, [Polynomial o k n])]
checks fs dss = [ (f, ds)
                | f <- fs, ds <- dss
                , not $ f `checkQuotsRem` ds
                ]
