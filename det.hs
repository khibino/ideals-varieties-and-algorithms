{-# LANGUAGE ParallelListComp #-}

import Data.List

coExpand1 :: [a] -> [[a]]
coExpand1 xs =
  [ i ++ t
  | i  <- init (inits xs)
  | t  <- tail (tails xs)
  ]

det :: [[Int]] -> Int
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



main :: IO ()
main =
  print $ det
  [ [  6,   0,   0,  24,   0,   0,   0]
  , [-23,   6,   0, -69,  24,   0,   0]
  , [ 32, -23,   6,  64, -69,  24,   0]
  , [-19,  32, -23, -19,  64, -69,  24]
  , [  4, -19,  32,   0, -19,  64, -69]
  , [  0,   4, -19,   0,   0, -19,  64]
  , [  0,   0,   4,   0,   0,   0, -19]
  ]
