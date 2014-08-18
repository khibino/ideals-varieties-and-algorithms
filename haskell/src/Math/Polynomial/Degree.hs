{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Math.Polynomial.Degree
       ( Degrees', Degrees, primeDegrees
       , list, rev, subt, lcm', total
       ) where

import GHC.TypeLits (Nat, Sing, SingI, SingE, sing, fromSing)
import Data.Monoid (Monoid(..))
import Data.List (foldl', find)
import Data.Word (Word)
import Data.Array (Array, listArray, bounds, elems, (!))


newtype Degrees' (n :: Nat) a = Degrees' (Array Word a)

unDeg :: Degrees' n a -> Array Word a
unDeg (Degrees' a) = a

size :: Degrees' n a -> Word
size d = h - l + 1  where (l, h) = bounds $ unDeg d

list :: Degrees' n a -> [a]
list =  elems . unDeg

rev :: Degrees' n a -> [a]
rev d = [ a ! i | i <- [h, h - 1 .. l] ]  where
  a = unDeg d
  (l, h) = bounds a

instance Eq a => Eq (Degrees' n a) where
  x == y = list x == list y

unsafeFromList :: Integral a => Word -> [a] -> Degrees' n a
unsafeFromList n = Degrees' . listArray (0, n - 1) . (++ repeat 0)

primeDegrees' :: Integral a => Sing n -> [a] -> Degrees' n a
primeDegrees' s = unsafeFromList . fromInteger $ fromSing s

primeDegrees :: (SingI n, Integral a) => [a] -> Degrees' n a
primeDegrees =  primeDegrees' sing

liftDeg2 :: Integral a => (a -> a -> a) -> Degrees' n a -> Degrees' n a -> Degrees' n a
liftDeg2 op x y = unsafeFromList (size x) $ zipWith op (list x) (list y)

instance (Integral a, SingI n) => Monoid (Degrees' n a) where
  mempty       =  primeDegrees []
  mappend      =  liftDeg2 (+)

instance Show a => Show (Degrees' n a) where
  show = show . list

instance (Read a, Integral a, SingI n) => Read (Degrees' n a) where
  readsPrec l s = [ (primeDegrees x, a) | (x, a) <- readsPrec l s ]

{-# SPECIALIZE subt :: Degrees' n Int -> Degrees' n Int -> Maybe (Degrees' n Int) #-}
subt :: (Integral a, Ord a) => Degrees' n a -> Degrees' n a -> Maybe (Degrees' n a)
subt x y
  | find (< 0) (list sub) == Nothing  =  Just sub
  | otherwise                         =  Nothing
  where sub = liftDeg2 (-) x y

{-# SPECIALIZE lcm' :: Degrees' n Int -> Degrees' n Int -> Degrees' n Int #-}
lcm' :: (Integral a, Ord a) => Degrees' n a -> Degrees' n a -> Degrees' n a
lcm' =  liftDeg2 max

{-# SPECIALIZE sum' :: [Int] -> Int #-}
sum' :: Num a => [a] -> a
sum' =  foldl' (+) 0

{-# SPECIALIZE total :: Degrees' n Int -> Int #-}
total :: Num a => Degrees' n a -> a
total =  sum' . list

type Degrees n = Degrees' n Int
