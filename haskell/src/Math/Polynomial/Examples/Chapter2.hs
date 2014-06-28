{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Math.Polynomial.Examples.Chapter2
       where

import Prelude hiding ((^))
import qualified Prelude
import Math.Polynomial.Imports
import Math.Polynomial.Check


(^) :: Num a => a -> Int -> a
(^) =  (Prelude.^)

eC2S3_1 :: DegreeOrder o => Polynomial o Rational 3
eC2S3_1 =  "x"^7 * "y"^2 + "x"^3 * "y"^2 - "y" + 1

eC2S3_1_gr :: Polynomial GrLex Rational 3
eC2S3_1_gr =  eC2S3_1

eC2S3_1_lex :: Polynomial Lex Rational 3
eC2S3_1_lex =  eC2S3_1

eC2S3_1a_F :: DegreeOrder o => [Polynomial o Rational 3]
eC2S3_1a_F =  ["x"*"y"^2 - "x", "x" - "y"^3]

-- eC2S3_1a_qr :: DegreeOrder o => (PolyQuots o Rational 3, Polynomial o Rational 3)
-- eC2S3_1a_qr =  eC2S3_1 /. eC2S3_1a_F

eC2S3_1a_qr_gr :: (PolyQuots GrLex Rational 3, Polynomial GrLex Rational 3)
eC2S3_1a_qr_gr =  eC2S3_1_gr /. eC2S3_1a_F

eC2S3_1a_qr_lex :: (PolyQuots Lex Rational 3, Polynomial Lex Rational 3)
eC2S3_1a_qr_lex =  eC2S3_1_lex /. eC2S3_1a_F

ppr_eC2S3_1a_qr_gr :: Pretty
ppr_eC2S3_1a_qr_gr =  pprQuotsRem eC2S3_1a_qr_gr

ppr_eC2S3_1a_qr_lex :: Pretty
ppr_eC2S3_1a_qr_lex = pprQuotsRem eC2S3_1a_qr_lex


eC2S3_1b_F :: DegreeOrder o => [Polynomial o Rational 3]
eC2S3_1b_F =  ["x" - "y"^3, "x"*"y"^2 - "x"]

eC2S3_1b_qr_gr :: (PolyQuots GrLex Rational 3, Polynomial GrLex Rational 3)
eC2S3_1b_qr_gr =  eC2S3_1_gr /. eC2S3_1b_F

eC2S3_1b_qr_lex :: (PolyQuots Lex Rational 3, Polynomial Lex Rational 3)
eC2S3_1b_qr_lex =  eC2S3_1_lex /. eC2S3_1b_F

ppr_eC2S3_1b_qr_gr :: Pretty
ppr_eC2S3_1b_qr_gr =  pprQuotsRem eC2S3_1b_qr_gr

ppr_eC2S3_1b_qr_lex :: Pretty
ppr_eC2S3_1b_qr_lex = pprQuotsRem eC2S3_1b_qr_lex


checks_gr :: [(Polynomial GrLex Rational 3, [Polynomial GrLex Rational 3])]
checks_gr = checks [eC2S3_1_gr] [eC2S3_1a_F, eC2S3_1b_F]

checks_lex :: [(Polynomial Lex Rational 3, [Polynomial Lex Rational 3])]
checks_lex = checks [eC2S3_1_lex] [eC2S3_1a_F, eC2S3_1b_F]
