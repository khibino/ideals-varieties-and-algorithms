{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Polynomial.Combinator
       ( (^.), (*.), (.*), lcm', (/.), p
       ) where

import GHC.TypeLits (SingI)
import Numeric (readDec)
import Data.Monoid ((<>))
import Data.String (IsString (..))

import Math.Polynomial.Degree (Degrees', primeDegrees)
import Math.Polynomial.Ord (DegreeOrder (..))

import Math.Polynomial.Data
  (Var, varNum, Mono, primeMono, Term, term, coeffMult, lcmTerm,
   Polynomial, polynomial, PolyQuotsRem, polyQuotRem)


varDegVector' :: (SingI n, Integral a) => Var k n -> a -> [a]
varDegVector' v a =  replicate (varNum v) 0 ++ [a]

varDegVector :: (SingI n, Integral a) => Var k n -> [a]
varDegVector =  (`varDegVector'` 1)

varDegrees :: (SingI n, Integral a) => Var k n -> Degrees' n a
varDegrees =  primeDegrees . varDegVector

instance (SingI n, Integral a) => IsString (Degrees' n a) where
  fromString = varDegrees . fromString

varMono :: SingI n => Var k n -> Mono k n
varMono =  primeMono . varDegVector

instance SingI n => IsString (Mono k n) where
  fromString = varMono . fromString

varTerm :: (SingI n, Num k) => Var k n -> Term k n
varTerm =  term 1 . varDegVector

instance (SingI n, Eq k, Num k) => IsString (Term k n) where
  fromString s = case [i | (i, "") <- readDec s] of
    []   ->  varTerm $ fromString s
    i:_  ->  term i []

varPolynomial :: (SingI n, Num k, Ord k, DegreeOrder o) => Var k n -> Polynomial o k n
varPolynomial =  polynomial . (:[]) . varTerm

instance (SingI n, Num k, Ord k, DegreeOrder o) => IsString (Polynomial o k n) where
  fromString s = case [i | (i, "") <- readDec s] of
    []   ->  varPolynomial $ fromString s
    i:_  ->  fromInteger i

(^.) :: (SingI n, Num k) => Var k n -> Int -> Term k n
v ^. e = term 1 $ varDegVector' v e

(*.) :: (SingI n, Num k) => Term k n -> Term k n -> Term k n
(*.) =  (<>)

(.*) :: Num k => k -> Term k n -> Term k n
(.*) =  coeffMult

lcm' :: Num k => Term k n -> Term k n -> Term k n
lcm' =  lcmTerm

p :: (Ord k, Num k, DegreeOrder o) => [Term k n] -> Polynomial o k n
p =  polynomial

(/.) :: (Fractional k, Ord k, SingI n, DegreeOrder o)
     => Polynomial o k n
     -> [Polynomial o k n]
     -> PolyQuotsRem o k n
(/.) = polyQuotRem

infixr 8 ^.
infixr 7 *., .*
infixl 5 /., `lcm'`
