{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}

module Math.Polynomial.Pretty
       ( Doc
       , pprDegrees, pprMono, pprTerm
       , pprPoly, pprPolyO, pprQuots, pprQuotsRem', pprQuotsRem
       )
       where

import GHC.TypeLits (SingI)
import Data.Monoid (Monoid(..), (<>))
import Data.Ratio (Ratio, numerator, denominator)

import Text.PrettyPrint.ANSI.Leijen
  (Doc, Pretty (..), text, comma, parens, (<+>),
   green, magenta, cyan,
   tupled, encloseSep, lbracket, rbracket, line)

import Math.Polynomial.Degree
  (Degrees', Degrees, primeDegrees)
import qualified Math.Polynomial.Degree as Degree
import Math.Polynomial.Ord (DegOrder2)
import Math.Polynomial.Data
  (Mono, degrees, primeMono, monoSing, Term, coeff, mono,
   variables, Polynomial, terms, PolyQuot (..), PolyQuotsRem (..))


pshow :: Show a => a -> Doc
pshow =  text . show

opt :: String -> Doc
opt =  green . text

binPpr' :: Doc -> Doc -> Doc -> Doc
binPpr' op x y = x <> op <> y

-- binPpr :: String -> Doc -> Doc -> Doc
-- binPpr =  binPpr' . string

pr :: (Doc -> Doc) -> String -> Doc
pr c = c . text

cparens :: (Doc -> Doc) -> Doc -> Doc
cparens c t = pr c "(" <> t <> pr c ")"

instance (Pretty a, Ord a, Integral a) => Pretty (Ratio a)  where
  pretty r
    | int && n < 0  =  cparens cyan $ pretty n
    | int           =  pretty n
    | otherwise     =  cparens cyan $ pretty n <> opt "/" <> pretty d   where
    n = numerator   r
    d = denominator r
    int = d == 1

pprDegrees :: Show a => Degrees' n a -> Doc
pprDegrees =  parens . d . Degree.list  where
  d []            =  mempty
  d [x]           =  pshow x
  d (x:xs@(_:_))  =  pshow x <> comma <> d xs

_e0Degrees :: Degrees 3
_e0Degrees =  primeDegrees [2, 1, 5]

_e1Degrees :: Degrees 1
_e1Degrees =  primeDegrees [2, 1, 5]

_e2Degrees :: Degrees 0
_e2Degrees =  primeDegrees [2, 1, 5]

pprMono :: SingI n => Mono k n -> Doc
pprMono m = fold
            [ var `pow` deg
            | (var, deg) <- zip vs ds
            , deg > 0
            ]                     where
  fold []        =  text "1"
  fold ts@(_:_)  =  foldr1 (<+>) ts
  vs  = variables  $ monoSing m
  ds  = Degree.list $ degrees m
  pow v n' = hat n' $ pshow v  where
    hat n
      | n == 1     =  id
      | otherwise  =  (<> opt "^" <> pshow n)

_e0Mono :: Mono Rational 3
_e0Mono =  mempty

_e1Mono :: Mono Rational 3
_e1Mono =  primeMono [4, 2, 1]

_e2Mono :: Mono Rational 3
_e2Mono =  primeMono [5, 0, 2]

pprTerm :: (Eq k, Num k, Pretty k, SingI n) => Term k n -> Doc
pprTerm t = coeff t `mul` mono t  where
  mul c m
    | c == 1      = pprMono m
    | m == mempty = pretty c
    | otherwise   = pretty c <> text " " <> pprMono m

pprPoly :: (Eq k, Num k, Pretty k, SingI n) => Polynomial o k n -> Doc
pprPoly p = fold [ pprTerm t | t <- terms p ]  where
  fold []        =  text "0"
  fold ts@(_:_)  =  foldr1 (binPpr' $ text " + ") ts

pprPolyO :: (Eq k, Num k, Pretty k, SingI n) => DegOrder2 o n -> Polynomial o k n -> Doc
pprPolyO =  const pprPoly

pprQuots :: (Eq k, Num k, Pretty k, SingI n) => [PolyQuot o k n] -> Doc
pprQuots =  encloseSep lbracket rbracket line . map pquot  where
  pquot pq = tupled [pprPoly $ quotient pq, pprPoly $ divisor pq]

pprQuotsRem' :: (Eq k, Num k, Pretty k, SingI n)
            => [PolyQuot o k n]
            -> Polynomial o k n
            -> Doc
pprQuotsRem' qs r = foldr1 (binPpr' $ opt " + " <> line) $ map pquot qs ++ [ppoly r]  where
  ppoly = cparens magenta . pprPoly
  pquot pq = ppoly (quotient pq) <> opt " * " <> ppoly (divisor pq)

pprQuotsRem :: (Eq k, Num k, Pretty k, SingI n)
            => PolyQuotsRem o k n
            -> Doc
pprQuotsRem qr =  pprQuotsRem' qs r  where
  qs = quots qr
  r  = remainder qr
