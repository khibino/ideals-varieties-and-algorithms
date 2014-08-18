{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Math.Polynomial.Examples.Small
       where

import Prelude hiding ((^))
import qualified Prelude
import Data.Monoid (mempty, (<>))
import Math.Polynomial.Imports


e0Degrees :: Degrees 3
e0Degrees =  primeDegrees [2, 1, 5]

e1Degrees :: Degrees 1
e1Degrees =  primeDegrees [2, 1, 5]

e2Degrees :: Degrees 0
e2Degrees =  primeDegrees [2, 1, 5]

e0Mono :: Mono Rational 3
e0Mono =  mempty

e1Mono :: Mono Rational 3
e1Mono =  primeMono [4, 2, 1]

e2Mono :: Mono Rational 3
e2Mono =  primeMono [5, 0, 2]

eLex :: Ordering
eLex =  monoCompare ordLex e0Mono e1Mono

eGrLex :: Ordering
eGrLex =  monoCompare ordGrLex e0Mono e1Mono

eGrevLex :: Ordering
eGrevLex =  e1Mono `compare` e2Mono


e0Term :: Term Rational 2
e0Term =  "y"

e1Term :: Term Rational 2
e1Term =  "z"

e2Term :: Term Rational 2
e2Term =  "1"

e3Term :: Term Rational 2
e3Term =  e0Term <> e1Term

e0Lcm :: Term Rational 3
e0Lcm =  "x" ^. 2 *. "y" `lcm'` "x" *. "y" ^. 3


e0Poly :: GPolynomial Rational 3
e0Poly =  1

e1Poly :: GPolynomial Rational 2
e1Poly =  "y" + 1

e2Poly :: GPolynomial Rational 2
e2Poly =  "z" + 1

e3Poly :: GPolynomial Rational 2
e3Poly =  1 + "z"


e0R :: GPolynomial Rational 1
e0Q :: [PolyQuot GrevLex Rational 1]
PolyQuotsRem e0Q e0R = p ["z"^.3, "1"] /. [p ["z", "1"]]

e4Poly :: Polynomial GrevLex Rational 3
e4Poly =  "x"^10 + 1  where
  (^) :: Num a => a -> Int -> a
  (^) =  (Prelude.^)

ePQR0 :: PolyQuotsRem GrevLex Rational 3
ePQR0 =  e4Poly /. [2 * "x" + 1]
