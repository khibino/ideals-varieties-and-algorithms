{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Math.Polynomial.Degree
       ( Degrees', Degrees, primeDegrees, list
       , degreeSubt, total
       , liftDeg2
       ) where

import GHC.TypeLits (Nat, Sing, SingI, SingE, sing, fromSing)
import Control.Applicative ((<$>), (<*>), ZipList (..))
import Data.Monoid (Monoid(..))
import Data.List (foldl', find)


newtype Degrees' (n :: Nat) d = Degrees' (ZipList d)

list :: Degrees' n a -> [a]
list (Degrees' (ZipList x)) = x

instance Eq a => Eq (Degrees' n a) where
  x == y = list x == list y

primeDegrees' :: Integral a => Sing n -> [a] -> Degrees' n a
primeDegrees' s = Degrees' . ZipList . take (fromInteger $ fromSing s) . (++ repeat 0)

primeDegrees :: (SingI n, Integral a) => [a] -> Degrees' n a
primeDegrees = primeDegrees' sing

liftDeg2 :: (a -> a -> a) -> Degrees' n a -> Degrees' n a -> Degrees' n a
liftDeg2 op (Degrees' x) (Degrees' y) = Degrees' $ op <$> x <*> y

instance (Integral a, SingI n) => Monoid (Degrees' n a) where
  mempty       =  primeDegrees []
  mappend      =  liftDeg2 (+)

instance Show a => Show (Degrees' n a) where
  show = show . list

instance (Read a, Integral a, SingI n) => Read (Degrees' n a) where
  readsPrec l s = [ (primeDegrees x, a) | (x, a) <- readsPrec l s ]

{-# SPECIALIZE degreeSubt :: Degrees' n Int -> Degrees' n Int -> Maybe (Degrees' n Int) #-}
degreeSubt :: (Num a, Ord a) => Degrees' n a -> Degrees' n a -> Maybe (Degrees' n a)
degreeSubt x y
  | find (< 0) (list sub) == Nothing = Just sub
  | otherwise                              = Nothing
  where sub = liftDeg2 (-) x y

{-# SPECIALIZE sum' :: [Int] -> Int #-}
sum' :: Num a => [a] -> a
sum' =  foldl' (+) 0

{-# SPECIALIZE total :: Degrees' n Int -> Int #-}
total :: Num a => Degrees' n a -> a
total =  sum' . list

type Degrees n = Degrees' n Int
