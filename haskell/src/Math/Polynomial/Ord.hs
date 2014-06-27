{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Polynomial.Ord
       ( invCompare, chainOrdering

       , DegOrder2' (..), DegOrder2
       , Lex, ordLex
       , GrLex, ordGrLex
       , GrevLex, ordGrevLex

       , DegreeOrder (..)

       , DegOrd)
       where

import GHC.TypeLits (Nat)
import Data.Function (on)
import Data.Monoid (mempty)
import Data.Ord (comparing)

import Math.Polynomial.Degree
  (Degrees', Degrees, primeDegrees, liftDeg2)
import qualified Math.Polynomial.Degree as Degree


invOrd' :: Ordering -> Ordering
invOrd' =  (EQ `compare`)

invCompare :: (a -> b -> Ordering) -> a -> b -> Ordering
invCompare c x y = invOrd' $ x `c` y

chainOrdering :: Ordering -> Ordering -> Ordering
chainOrdering c0 c1
  | c0 /= EQ  = c0
  | otherwise = c1

data Lex
data GrLex
data GrevLex

newtype DegOrder2' o (n :: Nat) a =
  DegOrder2 { degCompare :: Degrees' n a -> Degrees' n a -> Ordering }

-- degRevCompare :: DegOrder2' o n a -> Degrees' n a -> Degrees' n a -> Ordering
-- degRevCompare o x y = invCompare (degCompare o) x y where

ordLex :: Ord a => DegOrder2' Lex n a
ordLex =  DegOrder2 $ comparing Degree.list

ordGrLex :: (Ord a, Num a) => DegOrder2' GrLex n a
ordGrLex =  DegOrder2 comp  where
  x `comp` y
    | x   ==  y            = EQ
    | sx   >  sy           = GT
    | sx   <  sy           = LT
    | otherwise            = comparing Degree.list x y
      where sx = Degree.total x
            sy = Degree.total y

ordGrevLex :: (Ord a, Num a) => DegOrder2' GrevLex n a
ordGrevLex =  DegOrder2 comp  where
  x `comp` y
    | x  ==  y             =     EQ
    | sx  >  sy            =     GT
    | sx  <  sy            =     LT
    | otherwise = case grevComp of
        []                 ->    EQ
        (v:_) | v <  0     ->    GT
              | otherwise  ->    LT
    where sx = Degree.total x
          sy = Degree.total y
          grevComp = dropWhile (== 0) . reverse . Degree.list
                     $ liftDeg2 (-) x y

type DegOrder2 o n = DegOrder2' o n Int

class DegreeOrder o where
  degreeOrder :: (Ord a, Num a) => DegOrder2' o n a

instance DegreeOrder Lex where
  degreeOrder = ordLex

instance DegreeOrder GrLex where
  degreeOrder = ordGrLex

instance DegreeOrder GrevLex where
  degreeOrder = ordGrevLex


newtype DegOrd o (n :: Nat) a =
  DegOrd { ordDegrees :: Degrees' n a } deriving Eq

order2ord :: DegOrder2' o n a -> DegOrd o n a -> DegOrd o n a -> Ordering
order2ord o =  degCompare o `on` ordDegrees

instance (Ord a, Num a, DegreeOrder o) => Ord (DegOrd o n a) where
  compare = order2ord degreeOrder


-- Default ordering of degrees
instance (Ord a, Num a) => Ord (Degrees' n a) where
  compare  = degCompare ordGrevLex


_e0Deg :: Degrees 2
_e0Deg =  mempty

_e1Deg :: Degrees 3
_e1Deg =  primeDegrees [4, 2, 1]

_e2Deg :: Degrees 3
_e2Deg =  primeDegrees [5, 0, 2]
