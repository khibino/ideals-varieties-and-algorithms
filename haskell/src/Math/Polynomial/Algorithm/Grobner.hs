module Math.Polynomial.Algorithm.Grobner
       ( syzygyPairs, sPairCriterion
       , buchberger', reduce, buchberger
       , BuchStep, buchbergerSteps'
       ) where

import Data.Monoid ((<>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sortBy, tails)
import Data.Function (on)
import Control.Monad (foldM, when)
import GHC.TypeLits (SingI)

import Math.Polynomial.Ord (DegreeOrder, invCompare, ordGrLex)
import Math.Polynomial.Data
  (Polynomial, syzygyPolynomial, polyNormalize, Mono, leadingMono, monoLcm, monoDiv, monoCompare)
import Math.Polynomial.Algorithm.Division (polyQuotRem, remainder)


monoCoPrime :: SingI n => Mono k n -> Mono k n -> Bool
monoCoPrime m0 m1 = m0 <> m1 == monoLcm m0 m1

needDivTest :: (Fractional k, Ord k, SingI n, DegreeOrder o)
            => Polynomial o k n
            -> Polynomial o k n
            -> Maybe (Polynomial o k n)
needDivTest f0 f1 = do
  m0 <- leadingMono f0
  m1 <- leadingMono f1
  when (monoCoPrime m0 m1) Nothing
  syzygyPolynomial f0 f1

syzygyPairs :: (Fractional k, Ord k, SingI n, DegreeOrder o)
            => [Polynomial o k n]
            -> [Polynomial o k n]
syzygyPairs fs =
  catMaybes [
    needDivTest f0 f1
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
  | otherwise      =  (gs, mapMaybe (needDivTest r) ds ++ rs)
  where r = remainder $ f `polyQuotRem` ds
        (gs, rs) = divLoop fs (r:ds)

sPairCriterion :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => [Polynomial o k n]
             -> Bool
sPairCriterion fs = null . snd $ divLoop (syzygyPairs fs) fs

buchberger' :: (Fractional k, Ord k, SingI n, DegreeOrder o)
            => [Polynomial o k n]
            -> [Polynomial o k n]
buchberger'       []   =  []
buchberger' fs@(_:_)  =  loop (syzygyPairs fs) fs where
  loop sps ds0
    | rsps == []  =  ds1
    | otherwise   =  loop rsps ds1
    where (ds1, rsps) = divLoop sps ds0
  -- (gbs, ys)  =  divLoop (syzygyPairs fs ++ ys) fs

type BuchStep o k n = ([Polynomial o k n], (Polynomial o k n, Polynomial o k n))

divLoopSteps :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => [Polynomial o k n]
             -> [Polynomial o k n]
             -> ([Polynomial o k n], [BuchStep o k n])
divLoopSteps   []     ds  =  (ds, [])
divLoopSteps  (f:fs)  ds
  | r == 0         =  divLoopSteps fs ds
  | otherwise      =  (gs, (mapMaybe (syzygyPolynomial r) ds, (f, r)) : rs)
  where r = remainder $ f `polyQuotRem` ds
        (gs, rs) = divLoopSteps fs (r:ds)

buchbergerSteps' :: (Fractional k, Ord k, SingI n, DegreeOrder o)
                 => [Polynomial o k n]
                 -> ([Polynomial o k n], [[BuchStep o k n]])
buchbergerSteps'       []  =  ([], [])
buchbergerSteps' fs@(_:_)  =  loop (syzygyPairs fs) fs where
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


syzygyPairsIx :: (Fractional k, Ord k, SingI n, DegreeOrder o)
              => [Polynomial o k n]
              -> [((Int, Int), Polynomial o k n)]
syzygyPairsIx fs =
  catMaybes [
    do sp <- needDivTest f0 f1
       return ((i, j), sp)
    | ((i, f0), f1s) <- reverse . zip ifs . tail . tails $ ifs
    , (j, f1) <- f1s
    ]
  where (is, ris) = splitAt (length fs) [0..]
        ifs = zip is fs
