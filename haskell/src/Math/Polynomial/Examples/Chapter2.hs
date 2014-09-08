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

sec3_1a_F :: DegreeOrder o => [Polynomial o Rational 3]
sec3_1a_F =  ["x"*"y"^2 - "x", "x" - "y"^3]

sec3_1a_qr_gr :: PolyQuotsRem GrLex Rational 3
sec3_1a_qr_gr =  sec3_1 /. sec3_1a_F

sec3_1a_qr_lex :: PolyQuotsRem Lex Rational 3
sec3_1a_qr_lex =  sec3_1 /. sec3_1a_F


sec3_1b_F :: DegreeOrder o => [Polynomial o Rational 3]
sec3_1b_F =  ["x" - "y"^3, "x"*"y"^2 - "x"]

sec3_1b_qr_gr :: PolyQuotsRem GrLex Rational 3
sec3_1b_qr_gr =  sec3_1 /. sec3_1b_F

sec3_1b_qr_lex :: PolyQuotsRem Lex Rational 3
sec3_1b_qr_lex =  sec3_1 /. sec3_1b_F

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

sec7_2a_lex :: [Polynomial Lex Rational 3]
sec7_2a_lex =  ["x" ^ 2 * "y" - 1, "x" * "y"^2 - "x"]

sec8_4 :: DegreeOrder o => [Polynomial o Rational 3]
sec8_4 =
  [ "x"^2 * "y" - "z"^3
  , 2*"x"*"y" - 4*"z" - 1
  , "z" - "y"^2
  , "x"^3 - 4*"z"*"y"
  ]

sec8_4lexG :: [Polynomial Lex Rational 3]
sec8_4lexG =  buchberger sec8_4

sec8_4grevLexG :: [Polynomial GrevLex Rational 3]
sec8_4grevLexG =  buchberger sec8_4

sec8_4grLexG :: [Polynomial GrLex Rational 3]
sec8_4grLexG =  buchberger sec8_4

sec8_5 :: [Polynomial GrLex Rational 3]
sec8_5 =
  [ 2*"x"*("x"^2 + "y"^2 - 1) + ("x"^2 + "y"^2 - 4)* 2*"x" + (2 * "x" - 3)
  , 2*"y"*("x"^2 + "y"^2 - 1) + ("x"^2 + "y"^2 - 4)* 2*"y" + (2 * "y" - 3)
  ]

sec8_5GrLexG :: [Polynomial GrLex Rational 3]
sec8_5GrLexG =  buchberger sec8_5

{-
  f = (x - 1)^2 + (y - 1)^2 + (z - 1)^2
  g = x^4 + y^2 + z^2 - 1

  ∇f = w∇g
  2x - 2 = 4wx^3
  2y - 2 = 2wy
  2z - 2 = 2wz
  x^4 + y^2 + z^2 - 1 = 0
 -}

sec8_10 :: DegreeOrder o => [Polynomial o Rational 4]
sec8_10 =
  [ 2*"x" - 2 - 4*"w"*"x"^3
  , 2*"y" - 2 - 2*"w"*"y"
  , 2*"z" - 2 - 2*"w"*"z"
  , "x"^4 + "y"^2 + "z"^2 - 1
  ]

sec8_10lexG :: [Polynomial Lex Rational 4]
sec8_10lexG =  buchberger sec8_10

sec8_10grLexG :: [Polynomial GrLex Rational 4]
sec8_10grLexG =  buchberger sec8_10

sec8_10grevLexG :: [Polynomial GrevLex Rational 4]
sec8_10grevLexG =  buchberger sec8_10

{-

x * z^3 + (-2) * x * z^2 + z^3 + (1/2) * w * x + (3/8) * x * z + (-2) * z^2 + (1/2) * w + (3/4) * x + (3/8) * z + (1/2)
z^4 + (-1/2) * x * z^2 + (-2) * z^3 + (1/4) * w^2 + (1/4) * w * x + (1/2) * x * z + (-1/8) * z^2 + (1/8) * w + (1/16) * x + (9/4) * z + (-17/16)
w^3 + 2 * x * z^2 + (-4) * z^3 + (-3/2) * w^2 + (-5/4) * w * x + (-2) * x * z + 10 * z^2 + (-19/4) * w + (-7/2) * z + (-3)
w^2 * x + (-2) * x * z^2 + w^2 + (1/2) * w * x + 4 * x * z + (-2) * z^2 + (-9/4) * x + 4 * z + (-7/4)
w * z + (-1) * z + 1
x^2 + 4 * z^2 + (-2) * w + (-1) * x + (-4) * z
y + (-1) * z

x * t^3 + (-2) * x * t^2 + t^3 + (1/2) * w * x + (3/8) * x * t + (-2) * t^2 + (1/2) * w + (3/4) * x + (3/8) * t + (1/2)
t^4 + (-1/2) * x * t^2 + (-2) * t^3 + (1/4) * w^2 + (1/4) * w * x + (1/2) * x * t + (-1/8) * t^2 + (1/8) * w + (1/16) * x + (9/4) * t + (-17/16)
w^3 + 2 * x * t^2 + (-4) * t^3 + (-3/2) * w^2 + (-5/4) * w * x + (-2) * x * t + 10 * t^2 + (-19/4) * w + (-7/2) * t + (-3)
w^2 * x + (-2) * x * t^2 + w^2 + (1/2) * w * x + 4 * x * t + (-2) * t^2 + (-9/4) * x + 4 * t + (-7/4)
w * t + (-1) * t + 1
x^2 + 4 * t^2 + (-2) * w + (-1) * x + (-4) * t

 w = 1 - 1/t
 w = (t - 1)/t

1. x * t^3 + (-2) * x * t^2 + t^3 + (1/2) * w * x + (3/8) * x * t + (-2) * t^2 + (1/2) * w + (3/4) * x + (3/8) * t + (1/2)
2. t^4 + (-1/2) * x * t^2 + (-2) * t^3 + (1/4) * w^2 + (1/4) * w * x + (1/2) * x * t + (-1/8) * t^2 + (1/8) * w + (1/16) * x + (9/4) * t + (-17/16)
3. w^3 + 2 * x * t^2 + (-4) * t^3 + (-3/2) * w^2 + (-5/4) * w * x + (-2) * x * t + 10 * t^2 + (-19/4) * w + (-7/2) * t + (-3)
4. w^2 * x + (-2) * x * t^2 + w^2 + (1/2) * w * x + 4 * x * t + (-2) * t^2 + (-9/4) * x + 4 * t + (-7/4)
5. x^2 + 4 * t^2 + (-2) * w + (-1) * x + (-4) * t

1. x * t^3
 + (-2) * x * t^2
 + t^3
 + (1/2) * w * x
 + (3/8) * x * t
 + (-2) * t^2
 + (1/2) * w
 + (3/4) * x
 + (3/8) * t
 + (1/2)

(t^3 + (-2) * t^2 + (1/2) * w +  )

-}

sec8Ex1 :: DegreeOrder o => [Polynomial o Rational 3]
sec8Ex1 =  [ "x"*"z" - "y"^2, "x"^3 - "z"^2 ]

sec8Ex1grLexG :: [Polynomial GrLex Rational 3]
sec8Ex1grLexG =  buchberger sec8Ex1

sec8Ex5 :: DegreeOrder o => [Polynomial o Rational 7]
sec8Ex5 =
  [ "t" + "u" - "x"
  , "t"^2 + 2 * "t" * "u" - "y"
  , "t"^3 + 3 * "t"^2 * "u" - "z"
  ]

sec8Ex5lexG :: ([Polynomial Lex Rational 7], [[BuchStep Lex Rational 7]])
sec8Ex5lexG =  buchbergerSteps' sec8Ex5

sec8_11 :: DegreeOrder o => [Polynomial o Rational 3]
sec8_11 =
  [ "x" + "y" + "z" - 3
  , "x"^2 + "y"^2 + "z"^2 - 5
  , "x"^3 + "y"^3 + "z"^3 - 7
  ]

sec8_11grLex :: [Polynomial GrLex Rational 3]
sec8_11grLex =  buchberger sec8_11

sec8_11_f :: PolyQuotsRem GrLex Rational 3
sec8_11_f =
  "x"^4 + "y"^4 + "z"^4 - 9 /. sec8_11grLex

sec8_11_nf :: Int -> PolyQuotsRem GrLex Rational 3
sec8_11_nf n =
  "x"^n + "y"^n + "z"^n /. sec8_11grLex
