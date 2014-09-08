{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}

module Math.Polynomial.Pretty
       ( Doc, pretty
       , pprDegrees, pprMono, pprTerm
       , pprPoly, pprPolyO, pprQuot, pprQuotsRem', pprQuotsRem
       , pariPoly
       )
       where

import GHC.TypeLits (SingI)
import Data.Monoid (Monoid(..), (<>))
import Data.Ratio (Ratio, numerator, denominator)

import Text.PrettyPrint.ANSI.Leijen
  (Doc, Pretty (..), text, (<+>),
   green, magenta, cyan,
   tupled, line)

import Math.Polynomial.Degree (Degrees')
import qualified Math.Polynomial.Degree as Degree
import Math.Polynomial.Ord (DegOrder2)
import Math.Polynomial.Data
  (Mono, degrees, primeMono, monoSing, Term, coeff, mono,
   variables, Polynomial, terms)
import Math.Polynomial.Algorithm.Division (PolyQuot (..), PolyQuotsRem (..))


pshow :: Show a => a -> Doc
pshow =  text . show

opt :: String -> Doc
opt =  green . text

type BinOp = Doc -> Doc -> Doc

binPpr' :: Doc -> BinOp
binPpr' op x y = x <> op <> y

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

pprDegrees :: Pretty a => Degrees' n a -> Doc
pprDegrees =  tupled . map pretty . Degree.list  where

pprMono' :: SingI n => BinOp -> Mono k n -> Doc
pprMono' mulOp m =
  fold
  [ var `pow` deg
  | (var, deg) <- zip vs ds
  , deg > 0
  ]                     where
  fold []        =  text "1"
  fold ts@(_:_)  =  foldr1 mulOp ts
  vs  = variables  $ monoSing m
  ds  = Degree.list $ degrees m
  pow v n' = hat n' $ pshow v  where
    hat n
      | n == 1     =  id
      | otherwise  =  (<> opt "^" <> pshow n)

pprMono :: SingI n => Mono k n -> Doc
pprMono =  pprMono' (<+>)

_e0Mono :: Mono Rational 3
_e0Mono =  mempty

_e1Mono :: Mono Rational 3
_e1Mono =  primeMono [4, 2, 1]

_e2Mono :: Mono Rational 3
_e2Mono =  primeMono [5, 0, 2]

pprTerm' :: (Eq k, Num k, Pretty k, SingI n) => BinOp -> Term k n -> Doc
pprTerm' mulOp t = coeff t `mul` mono t  where
  mul c m
    | c == 1      = pprMono' mulOp m
    | m == mempty = pretty c
    | otherwise   = pretty c `mulOp` pprMono' mulOp m

pprTerm :: (Eq k, Num k, Pretty k, SingI n) => Term k n -> Doc
pprTerm =  pprTerm' (<+>)

pprPoly' :: (Eq k, Num k, Pretty k, SingI n) => BinOp -> Polynomial o k n -> Doc
pprPoly' mulOp p = fold [ pprTerm' mulOp t | t <- terms p ]  where
  fold []        =  text "0"
  fold ts@(_:_)  =  foldr1 (binPpr' $ text " + ") ts

pariPoly :: (Eq k, Num k, Pretty k, SingI n) => Polynomial o k n -> Doc
pariPoly =  pprPoly' (\a b -> a <> text " * " <> b)

pprPoly :: (Eq k, Num k, Pretty k, SingI n) => Polynomial o k n -> Doc
pprPoly = pprPoly' (<+>)

pprPolyO :: (Eq k, Num k, Pretty k, SingI n) => DegOrder2 o n -> Polynomial o k n -> Doc
pprPolyO =  const pprPoly

pprQuot :: (Eq k, Num k, Pretty k, SingI n) => PolyQuot o k n -> Doc
pprQuot pq = tupled [pprPoly $ quotient pq, pprPoly $ divisor pq]

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

instance Pretty a => Pretty (Degrees' n a) where
  pretty = pprDegrees

instance (Pretty k, SingI n) => Pretty (Mono k n) where
  pretty = pprMono

instance (Pretty k, Eq k, Num k, SingI n) => Pretty (Term k n) where
  pretty = pprTerm

instance (Pretty k, Eq k, Num k, SingI n) => Pretty (Polynomial o k n) where
  pretty = pprPoly

instance (Pretty k, Eq k, Num k, SingI n) => Pretty (PolyQuot o k n) where
  pretty = pprQuot

instance (Pretty k, Eq k, Num k, SingI n) => Pretty (PolyQuotsRem o k n) where
  pretty = pprQuotsRem
