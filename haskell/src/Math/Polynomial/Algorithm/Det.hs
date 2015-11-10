{-# LANGUAGE ParallelListComp #-}

module Math.Polynomial.Algorithm.Det
       (det) where

import Data.List

coExpand1 :: [a] -> [[a]]
coExpand1 xs =
  [ i ++ t
  | i  <- init (inits xs)
  | t  <- tail (tails xs)
  ]

det :: Num a => [[a]] -> a
det []       =  0
det [[x]]    =  x
det (xs:yss) =
  sum
  [ x * d * s
  | x  <- xs
  | d  <- map det . transpose $ map coExpand1 yss
  | s  <- cycle [1, -1]
  ]

_t22a :: Int
_t22a = det [[1,2], [3,4]]

_t33a :: Int
_t33a = det [[1,2,3], [1,2,3], [1,2,3]]
