module Math.Polynomial.Algorithm.Grobner
       ( sPairs, sPairCriterion
       , buchberger' ) where

import Data.Maybe (catMaybes, mapMaybe)
import Data.List (tails)
import GHC.TypeLits (SingI)

import Math.Polynomial.Ord (DegreeOrder)
import Math.Polynomial.Data (Polynomial, sPolynomial)
import Math.Polynomial.Algorithm.Division (polyQuotRem, remainder)


sPairs :: (Fractional k, Ord k, SingI n, DegreeOrder o)
       => [Polynomial o k n]
       -> [Polynomial o k n]
sPairs fs =
  catMaybes [
    sPolynomial f0 f1
    | (f0, f1s) <- reverse . zip fs . tail . tails $ fs
    , f1 <- f1s
    ]

divLoop :: (Fractional k, Ord k, SingI n, DegreeOrder o)
        => [Polynomial o k n]
        -> [Polynomial o k n]
        -> ([Polynomial o k n], [Polynomial o k n])
divLoop   []     ds  =  (ds, [])
divLoop  (f:fs)  ds
  | r == 0         =  divLoop fs ds
  | otherwise      =  (gs, mapMaybe (sPolynomial r) ds ++ rs)
  where r = remainder $ f `polyQuotRem` ds
        (gs, rs) = divLoop fs (r:ds)

sPairCriterion :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => [Polynomial o k n]
             -> Bool
sPairCriterion fs = null . snd $ divLoop (sPairs fs) fs

buchberger' :: (Fractional k, Ord k, SingI n, DegreeOrder o)
            => [Polynomial o k n]
            -> [Polynomial o k n]
buchberger'       []   =  []
buchberger' fs@(_:_)  =  loop (sPairs fs) fs where
  loop sps ds0
    | rsps == []  =  ds1
    | otherwise   =  loop rsps ds1
    where (ds1, rsps) = divLoop sps ds0
  -- (gbs, ys)  =  divLoop (sPairs fs ++ ys) fs
