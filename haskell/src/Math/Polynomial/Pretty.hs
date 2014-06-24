{-# LANGUAGE DataKinds #-}

module Math.Polynomial.Pretty
       ( Pretty
       , pprDegrees, pprMono, pprTerm
       , pprPoly, pprPolyO, pprQuots, pprQuotsRem', pprQuotsRem
       )
       where

import GHC.TypeLits (SingI)
import Data.Monoid (Monoid(..), (<>))

import Text.PrettyPrint.ANSI.Leijen
  (Doc, text, string, comma, parens, (<+>),
   tupled, encloseSep, lbracket, rbracket, line)

import Math.Polynomial.Degree
  (Degrees', Degrees, primeDegrees, degreeList)
import Math.Polynomial.Ord (DegOrder2)
import Math.Polynomial.Data
  (Mono, degrees, primeMono, monoSing, Term, coeff, mono,
   variables, Polynomial, terms, PolyQuots)


type Pretty = Doc

ppr :: Show a => a -> Pretty
ppr =  text . show

binPpr' :: Pretty -> Pretty -> Pretty -> Pretty
binPpr' op x y = x <> op <> y

binPpr :: String -> Pretty -> Pretty -> Pretty
binPpr =  binPpr' . string

pprDegrees :: Show a => Degrees' n a -> Pretty
pprDegrees =  parens . d . degreeList  where
  d []            =  mempty
  d [x]           =  ppr x
  d (x:xs@(_:_))  =  ppr x <> comma <> d xs

_e0Degrees :: Degrees 3
_e0Degrees =  primeDegrees [2, 1, 5]

_e1Degrees :: Degrees 1
_e1Degrees =  primeDegrees [2, 1, 5]

_e2Degrees :: Degrees 0
_e2Degrees =  primeDegrees [2, 1, 5]

pprMono :: SingI n => Mono k n -> Pretty
pprMono m = fold
            [ var `pow` deg
            | (var, deg) <- zip vs ds
            , deg > 0
            ]                     where
  fold []        =  text "1"
  fold ts@(_:_)  =  foldr1 (<+>) ts
  vs  = variables  $ monoSing m
  ds  = degreeList $ degrees m
  pow v n' = hat n' $ ppr v  where
    hat n
      | n == 1     =  id
      | otherwise  =  (<> text "^" <> ppr n)

_e0Mono :: Mono Rational 3
_e0Mono =  mempty

_e1Mono :: Mono Rational 3
_e1Mono =  primeMono [4, 2, 1]

_e2Mono :: Mono Rational 3
_e2Mono =  primeMono [5, 0, 2]

pprCoeff :: (Eq k, Num k, Show k) => k -> Pretty
pprCoeff c
  | c == 1      =  mempty
  | otherwise   =  uparen $ show c  where
    uparen s
      | ' ' `elem` s = parens $ text s
      | otherwise    = text s

pprTerm :: (Eq k, Num k, Show k, SingI n) => Term k n -> Pretty
pprTerm t = coeff t `mul` mono t  where
  mul c m
    | c == 1      = pprMono m
    | m == mempty = pprCoeff c
    | otherwise   = pprCoeff c <> text "*" <> pprMono m

pprPoly :: (Eq k, Num k, Show k, SingI n) => Polynomial o k n -> Pretty
pprPoly p = fold [ pprTerm t | t <- terms p ]  where
  fold []        =  text "0"
  fold ts@(_:_)  =  foldr1 (binPpr " + ") ts

pprPolyO :: (Eq k, Num k, Show k, SingI n) => DegOrder2 o n -> Polynomial o k n -> Pretty
pprPolyO =  const pprPoly

pprQuots :: (Eq k, Num k, Show k, SingI n) => PolyQuots o k n -> Pretty
pprQuots =  encloseSep lbracket rbracket line . map pquot  where
  pquot (d, q) = tupled [pprPoly d, pprPoly q]

pprQuotsRem' :: (Eq k, Num k, Show k, SingI n)
            => PolyQuots o k n
            -> Polynomial o k n
            -> Pretty
pprQuotsRem' qs r = foldr1 (binPpr' $ text " + " <> line) $ map pquot qs ++ [ppoly r]  where
  ppoly = parens . pprPoly
  pquot (d, q) = ppoly d <> text " * " <> ppoly q

pprQuotsRem :: (Eq k, Num k, Show k, SingI n)
            => (PolyQuots o k n, Polynomial o k n)
            -> Pretty
pprQuotsRem =  uncurry pprQuotsRem'
