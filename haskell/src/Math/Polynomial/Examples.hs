{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Math.Polynomial.Examples
       where

import Prelude hiding ((^))
import qualified Prelude
import Data.Monoid (mempty, (<>))
import Math.Polynomial.Imports
import Math.Polynomial.Check

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


e0Poly :: GPolynomial Rational 3
e0Poly =  1

e1Poly :: GPolynomial Rational 2
e1Poly =  "y" + 1

e2Poly :: GPolynomial Rational 2
e2Poly =  "z" + 1

e3Poly :: GPolynomial Rational 2
e3Poly =  1 + "z"


e0R :: GPolynomial Rational 1
e0Q :: [(GPolynomial Rational 1, GPolynomial Rational 1)]
(e0Q, e0R) = p ["z"^.3, "1"] /. [p ["z", "1"]]

(^) :: Num a => a -> Int -> a
(^) =  (Prelude.^)

eC2S3_1a :: DegreeOrder o => Polynomial o Rational 3
eC2S3_1a =  "x"^7 * "y"^2 + "x"^3 * "y"^2 - "y" + 1

eC2S3_1a_F :: DegreeOrder o => [Polynomial o Rational 3]
eC2S3_1a_F =  ["x"*"y"^2 - "x", "x" - "y"^3]

eC2S3_1a_qr :: DegreeOrder o => (PolyQuots o Rational 3, Polynomial o Rational 3)
eC2S3_1a_qr =  eC2S3_1a /. eC2S3_1a_F

eC2S3_1a_qr_gr :: (PolyQuots GrLex Rational 3, Polynomial GrLex Rational 3)
eC2S3_1a_qr_gr =  eC2S3_1a_qr

eC2S3_1a_qr_lex :: (PolyQuots Lex Rational 3, Polynomial Lex Rational 3)
eC2S3_1a_qr_lex =  eC2S3_1a_qr

ppr_eC2S3_1a_qr_gr :: Pretty
ppr_eC2S3_1a_qr_gr =  pprQuotsRem eC2S3_1a_qr_gr

ppr_eC2S3_1a_qr_lex :: Pretty
ppr_eC2S3_1a_qr_lex = pprQuotsRem eC2S3_1a_qr_lex

eC2S3_1a_gr :: Polynomial GrLex Rational 3
eC2S3_1a_gr =  eC2S3_1a

eC2S3_1a_lex :: Polynomial Lex Rational 3
eC2S3_1a_lex =  eC2S3_1a

checkC2S3_1a_gr :: Bool
checkC2S3_1a_gr = checkQuotsRem eC2S3_1a_gr eC2S3_1a_F

checkC2S3_1a_lex :: Bool
checkC2S3_1a_lex = checkQuotsRem eC2S3_1a_lex eC2S3_1a_F
