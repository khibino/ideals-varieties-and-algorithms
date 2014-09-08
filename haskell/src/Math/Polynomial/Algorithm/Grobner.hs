module Math.Polynomial.Algorithm.Grobner
       ( sPairs, sPairCriterion
       , buchberger', reduce, buchberger
       , BuchStep, buchbergerSteps'
       ) where

import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sortBy, tails)
import Data.Function (on)
import Control.Monad (foldM)
import GHC.TypeLits (SingI)

import Math.Polynomial.Ord (DegreeOrder, invCompare, ordGrLex)
import Math.Polynomial.Data
  (Polynomial, sPolynomial, polyNormalize, Mono, leadingMono, monoDiv, monoCompare)
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

type BuchStep o k n = ([Polynomial o k n], (Polynomial o k n, Polynomial o k n))

divLoopSteps :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => [Polynomial o k n]
             -> [Polynomial o k n]
             -> ([Polynomial o k n], [BuchStep o k n])
divLoopSteps   []     ds  =  (ds, [])
divLoopSteps  (f:fs)  ds
  | r == 0         =  divLoopSteps fs ds
  | otherwise      =  (gs, (mapMaybe (sPolynomial r) ds, (f, r)) : rs)
  where r = remainder $ f `polyQuotRem` ds
        (gs, rs) = divLoopSteps fs (r:ds)

buchbergerSteps' :: (Fractional k, Ord k, SingI n, DegreeOrder o)
                 => [Polynomial o k n]
                 -> ([Polynomial o k n], [[BuchStep o k n]])
buchbergerSteps'       []  =  ([], [])
buchbergerSteps' fs@(_:_)  =  loop (sPairs fs) fs where
  loop sps ds0
    | rsps == []  =  (ds1, [])
    | otherwise   =  let (gs, st) = loop rsps ds1 in (gs, steps : st)
    where (ds1, steps) = divLoopSteps sps ds0
          rsps = concat $ map fst steps

monoPair :: Fractional k => Polynomial o k n -> Maybe (Mono k n, Polynomial o k n)
monoPair f = do
  lm <- leadingMono f
  return (lm, polyNormalize f)

uncons :: [t] -> Maybe (t, [t])
uncons  []    = Nothing
uncons (x:xs) = Just (x, xs)

reduce :: Fractional k => [Polynomial o k n] -> [Polynomial o k n]
reduce fs = map snd $ rps where
  mps  = sortBy (invCompare (monoCompare ordGrLex `on` fst)) $ mapMaybe monoPair fs
  cps = mapMaybe uncons . tails $ mps
  rfilter pf@(lmF, _f) (lmG, _g) =
    maybe (Just pf) (const Nothing) $ lmF `monoDiv` lmG
  rps = mapMaybe (uncurry $ foldM rfilter) cps

buchberger :: (Fractional k, Ord k, SingI n, DegreeOrder o)
           => [Polynomial o k n]
           -> [Polynomial o k n]
buchberger =  reduce . buchberger'
