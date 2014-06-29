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

sec3_1 :: DegreeOrder o => Polynomial o Rational 3
sec3_1 =  "x"^7 * "y"^2 + "x"^3 * "y"^2 - "y" + 1

sec3_1_gr :: Polynomial GrLex Rational 3
sec3_1_gr =  sec3_1

sec3_1_lex :: Polynomial Lex Rational 3
sec3_1_lex =  sec3_1

sec3_1a_F :: DegreeOrder o => [Polynomial o Rational 3]
sec3_1a_F =  ["x"*"y"^2 - "x", "x" - "y"^3]

-- sec3_1a_qr :: DegreeOrder o => (PolyQuots o Rational 3, Polynomial o Rational 3)
-- sec3_1a_qr =  sec3_1 /. sec3_1a_F

sec3_1a_qr_gr :: PolyQuotsRem GrLex Rational 3
sec3_1a_qr_gr =  sec3_1_gr /. sec3_1a_F

sec3_1a_qr_lex :: PolyQuotsRem Lex Rational 3
sec3_1a_qr_lex =  sec3_1_lex /. sec3_1a_F

ppr_sec3_1a_qr_gr :: Doc
ppr_sec3_1a_qr_gr =  pprQuotsRem sec3_1a_qr_gr

ppr_sec3_1a_qr_lex :: Doc
ppr_sec3_1a_qr_lex =  pprQuotsRem sec3_1a_qr_lex


sec3_1b_F :: DegreeOrder o => [Polynomial o Rational 3]
sec3_1b_F =  ["x" - "y"^3, "x"*"y"^2 - "x"]

sec3_1b_qr_gr :: PolyQuotsRem GrLex Rational 3
sec3_1b_qr_gr =  sec3_1_gr /. sec3_1b_F

sec3_1b_qr_lex :: PolyQuotsRem Lex Rational 3
sec3_1b_qr_lex =  sec3_1_lex /. sec3_1b_F

ppr_sec3_1b_qr_gr :: Doc
ppr_sec3_1b_qr_gr =  pprQuotsRem sec3_1b_qr_gr

ppr_sec3_1b_qr_lex :: Doc
ppr_sec3_1b_qr_lex = pprQuotsRem sec3_1b_qr_lex


sec3_2 :: DegreeOrder o => Polynomial o Rational 3
sec3_2 =  "x"*"y"^2*"z"^2 + "x"*"y" - "y"*"z"

sec3_2a_F :: DegreeOrder o => [Polynomial o Rational 3]
sec3_2a_F =  ["x" - "y"^2, "y" - "z"^3, "z"^2 - 1]

sec3_2b_F :: DegreeOrder o => [Polynomial o Rational 3]
sec3_2b_F =  ["y" - "z"^3, "z"^2 - 1, "x" - "y"^2]

sec3_2a_qr_gr :: PolyQuotsRem GrLex Rational 3
sec3_2a_qr_gr =  sec3_2 /. sec3_2a_F

sec3_2a_qr_grev :: PolyQuotsRem GrevLex Rational 3
sec3_2a_qr_grev =  sec3_2 /. sec3_2a_F

sec3_2a_qr_lex :: PolyQuotsRem Lex Rational 3
sec3_2a_qr_lex =  sec3_2 /. sec3_2a_F

sec3_2b_qr_gr :: PolyQuotsRem GrLex Rational 3
sec3_2b_qr_gr =  sec3_2 /. sec3_2b_F

sec3_2b_qr_grev :: PolyQuotsRem GrevLex Rational 3
sec3_2b_qr_grev =  sec3_2 /. sec3_2b_F

sec3_2b_qr_lex :: PolyQuotsRem Lex Rational 3
sec3_2b_qr_lex =  sec3_2 /. sec3_2b_F


checks_gr :: [(Polynomial GrLex Rational 3, [Polynomial GrLex Rational 3])]
checks_gr =  checks [sec3_1, sec3_2] [sec3_1a_F, sec3_1b_F, sec3_2a_F, sec3_2b_F]

checks_grev :: [(Polynomial GrevLex Rational 3, [Polynomial GrevLex Rational 3])]
checks_grev =  checks [sec3_1, sec3_2] [sec3_1a_F, sec3_1b_F, sec3_2a_F, sec3_2b_F]

checks_lex :: [(Polynomial Lex Rational 3, [Polynomial Lex Rational 3])]
checks_lex =  checks [sec3_1, sec3_2] [sec3_1a_F, sec3_1b_F, sec3_2a_F, sec3_2b_F]
