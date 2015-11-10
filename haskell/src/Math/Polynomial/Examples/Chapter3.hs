{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Math.Polynomial.Examples.Chapter3
       where

import Prelude hiding ((^))
import qualified Prelude
import Math.Polynomial.Imports
import Math.Polynomial.Check


(^) :: Num a => a -> Int -> a
(^) =  (Prelude.^)

sec1_3 :: DegreeOrder o => [Polynomial o Rational 3]
sec1_3 =  [ "x"^2 + 2 * "y"^2 - 2
          , "x"^2 + "x" * "y" + "y"^2 - 2
          ]

sec1_3lex :: [Polynomial Lex Rational 3]
sec1_3lex =  buchberger sec1_3

sec1_4 :: DegreeOrder o => [Polynomial o Rational 3]
sec1_4 =  [ "x"^2 + "y"^2 + "z"^2 - 4
          , "x"^2 + 2*"y"^2 - 5
          , "x"*"z" - 1
          ]

sec1_4lex :: [Polynomial Lex Rational 3]
sec1_4lex =  buchberger sec1_4

sec1_7 :: DegreeOrder o => [Polynomial o Rational 4]
sec1_7 =  [ "w"^2 + "x"^2 + "y"^2 + "z"^2
          , "w"^2 + 2 * "x"^2 - "x" * "y" - "z"^2
          , "w" + "y"^3 - "z"^3
          ]

sec1_7lex :: [Polynomial Lex Rational 4]
sec1_7lex =  buchberger sec1_7

-- foo :: [Polynomial GrevLex Rational 3]
-- foo = [ "y"^12 + (-4) "y"^9 "z"^3 + 5 "y"^8 + 6 "y"^6 "z"^6 + 6 "y"^6 "z"^2 + (-10) "y"^5 "z"^3 + 5 "y"^4 + (-4) "y"^3 "z"^9 + (-12) "y"^3 "z"^5 + 5 "y"^2 "z"^6 + 13 "y"^2 "z"^2 + "z"^12 + 6 "z"^8 + 9 "z"^4
--       ,"x" "z"^6 + 3 "x" "z"^2 + (-1) "y"^11 + 4 "y"^8 "z"^3 + (-5) "y"^7 + (-5) "y"^5 "z"^6 + (-3) "y"^5 "z"^2 + 10 "y"^4 "z"^3 + (-5) "y"^3 + 2 "y"^2 "z"^9 + 6 "y"^2 "z"^5 + (-3) "y" "z"^6 + (-7) "y" "z"^2
--       ,"x"^2 + (-1/2) "x" * "y" + (1/2) "y"^6 + (-1) "y"^3 "z"^3 + (1/2) "z"^6 + (-1/2) "z"^2
--       ,"x" "y" + "y"^6 + (-2) "y"^3 "z"^3 + 2 "y"^2 + "z"^6 + 3 "z"^2
-- ---      ,"w" + "y"^3 + (-1) "z"^3
--       ]

sec1_9 :: DegreeOrder o => [Polynomial o Rational 3]
sec1_9 =  [ "x"^10 - "x"^5 * "y" + 1
          , "x"^2 - "x" * "z" + 1
          ]

sec1_9lex :: [Polynomial Lex Rational 3]
sec1_9lex =  buchberger sec1_9


sec2_4 :: DegreeOrder o => Polynomial o Rational 3
sec2_4 =  "x"^3 * "z" - constant (3/4) * "x"^2 * "y"^2 - constant (3/2) * "x" * "y" * "z" + "y" ^ 3 + constant (1/4) * "z"^2

sec2_4basis :: DegreeOrder o => [Polynomial o Rational 3]
sec2_4basis =  [ "x"^3 - "z", "x"^2 - "y" ]

sec2_4lex :: [Polynomial Lex Rational 3]
sec2_4lex =  buchberger sec2_4basis

sec2_4grlex :: [Polynomial GrLex Rational 3]
sec2_4grlex =  buchberger sec2_4basis

sec2_4grevlex :: [Polynomial GrevLex Rational 3]
sec2_4grevlex =  buchberger sec2_4basis

sec2_4div :: PolyQuotsRem Lex Rational 3
sec2_4div =  sec2_4 /. sec2_4lex

-- sec2_4lex :: [Polynomial Lex Rational 3]
-- sec2_4lex =  buchberger sec2_4

sec3_6 :: [Polynomial Lex Rational 6]
sec3_6 =  [ "x" - "u" * "v"
          , "y" - "u" ^ 2
          , "z" - "v" ^ 2
          ]

sec3_6lex :: [Polynomial Lex Rational 6]
sec3_6lex =  buchberger sec3_6

sec3_6c0 :: [Polynomial Lex Rational 6]
sec3_6c0 =  [ "x" - "u" * "v"
            , "y" + "u" ^ 2
            , "z" + "v" ^ 2
            ]


sec3_6c0_lex :: [Polynomial Lex Rational 6]
sec3_6c0_lex =  buchberger sec3_6c0

sec3_6c1 :: [Polynomial Lex Rational 6]
sec3_6c1 =  [ "x" + "u" * "v"
            , "y" + "u" ^ 2
            , "z" + "v" ^ 2
            ]


sec3_6c1_lex :: [Polynomial Lex Rational 6]
sec3_6c1_lex =  buchberger sec3_6c1


sec3_8 :: [Polynomial Lex Rational 6]
sec3_8 =  [ "x" - 3 * "u" - 3 * "y" * "v"^2 + "u"^3
          , "y" - 3 * "v" - 3 * "u"^2 * "v" + "v"^3
          , "z" - 3 * "u"^2  + 3 * "v"^2
          ]

sec3_8lex :: [Polynomial Lex Rational 6]
sec3_8lex =  buchberger sec3_8

sec3_9 :: [Polynomial Lex Rational 6]
sec3_9 =  [ "x" - "u" * "v"
          , "y" - "v"
          , "z" + "u" ^ 2
          ]

sec3_9lex :: [Polynomial Lex Rational 6]
sec3_9lex =  buchberger sec3_9


sec3_12 :: [Polynomial Lex Rational 6]
sec3_12 =  [ "v" * "x" - "u" ^ 2
           , "u" * "y" - "v" ^ 2
           , "z" - "u"
           ]

sec3_12lex :: [Polynomial Lex Rational 6]
sec3_12lex =  buchberger sec3_12


sec3_12J :: [Polynomial Lex Rational 7]
sec3_12J =  [ "v" * "x" - "u" ^ 2
            , "u" * "y" - "v" ^ 2
            , "z" - "u"
            , 1 - "u" * "v" * "t"
           ]

sec3_12J_lex :: [Polynomial Lex Rational 7]
sec3_12J_lex =  buchberger sec3_12J

sec3_12K :: [Polynomial Lex Rational 6]
sec3_12K =  [ "v" * "x" - "u" ^ 2
            , "u" * "y" - "v" ^ 2
            , "z" - "u"
            , "x"^2 * "y" - "z" ^ 3
            , "v" * "z" - "x" * "y"
           ]

sec3_12K_lex :: [Polynomial Lex Rational 6]
sec3_12K_lex =  buchberger sec3_12K


sec4_18_A2 :: Polynomial GrLex Rational 3
sec4_18_A2 = 7327 - 1928 * "y" - 768 * "y"^2 - 896 * "y"^3 + 256 * "y"^4

sec4_18_A3 :: Polynomial GrLex Rational 3
sec4_18_A3 = 431 * "x" - 12 * "x" * "y" - 48 * "x" * "y"^2 - 64 * "x" * "y"^3

sec4_18_A4 :: Polynomial GrLex Rational 3
sec4_18_A4 = 697 - 288 * "x"^2 + 108 * "y" - 336 * "y"^2 + 64 * "y"^3

sec4_18_A234 :: [Polynomial GrLex Rational 3]
sec4_18_A234 =
  [ sec4_18_A2
  , sec4_18_A3
  , sec4_18_A4
  ]

sec4_18_A234_grLex :: [Polynomial GrLex Rational 3]
sec4_18_A234_grLex = buchberger sec4_18_A234

sec4_18_g1 :: Polynomial GrLex Rational 3
sec4_18_g1 =
  - 1156 + 688 * "x"^2 - 191 * "x"^4 + 16 * "x"^6 + 544 * "y" + 30 * "x"^2 * "y" - 40 * "x"^4 * "y"
  + 225 * "y"^2 - 96 * "x"^2 * "y"^2 + 16 * "x"^4 * "y"^2 - 136 * "y"^3 - 32 * "x"^2 * "y"^3 + 16 * "y"^4

sec4_18_dg1dx :: Polynomial GrLex Rational 3
sec4_18_dg1dx =
  688 * 2 * "x" - 191 * 4 * "x"^3 + 16 * 6 * "x"^5 + 30 * 2 * "x" * "y" - 40 * 4 * "x"^3 * "y"
  - 96 * 2 * "x" * "y"^2 + 16 * 4 * "x"^3 * "y"^2 - 32 * 2 * "x" * "y"^3

sec4_18_dg1dy :: Polynomial GrLex Rational 3
sec4_18_dg1dy =
  544 + 30 * "x"^2 - 40 * "x"^4
  + 225 * 2 * "y" - 96 * "x"^2 * 2 * "y" + 16 * "x"^4 * 2 * "y"
  - 136 * 3 * "y"^2 - 32 * "x"^2 * 3 * "y"^2 + 16 * 4 * "y"^3

sec4_18_singular_equation :: [Polynomial GrLex Rational 3]
sec4_18_singular_equation  =
  [ sec4_18_g1, sec4_18_dg1dx, sec4_18_dg1dy ]

{- g₁, 𝟃(g₁)/𝟃x, 𝟃(g₁)/𝟃y ∈ ⟨A₂, A₃, A₄⟩ -}
sec4_18_singular_left :: Bool
sec4_18_singular_left =
  all (== 0)
  [ remainder $ f /. sec4_18_A234_grLex
  | f <- sec4_18_singular_equation
  ]

sec4_18_singular_equation_grLex :: [Polynomial GrLex Rational 3]
sec4_18_singular_equation_grLex = buchberger sec4_18_singular_equation

{- ⟨g₁, 𝟃(g₁)/𝟃x, 𝟃(g₁)/𝟃y⟩ ∋ A₂^2, A₃^2, A₄^2 -}
sec4_18_singular_right :: Bool
sec4_18_singular_right =
  all (== 0)
  [ remainder $ a^2 /. sec4_18_singular_equation_grLex
  | a <- sec4_18_A234
  ]

{- A₂ ∉ ⟨g₁, 𝟃(g₁)/𝟃x, 𝟃(g₁)/𝟃y⟩ -}
sec4_18_A2_in :: Bool
sec4_18_A2_in =
  not $ remainder (sec4_18_A2 /. sec4_18_singular_equation_grLex) == 0

sec5_6_ex_f :: Polynomial GrLex Rational 3
sec5_6_ex_f = "x"^4 + "x"^2 + 1

sec5_6_ex_f' :: Polynomial GrLex Rational 3
sec5_6_ex_f' = 4 * "x"^3 + 2 * "x"

sec5_6_ex :: [Polynomial GrLex Rational 3]
sec5_6_ex = buchberger [ sec5_6_ex_f, sec5_6_ex_f' ]

sec5_9_f :: Polynomial GrLex Rational 3
sec5_9_f = 6 * "x"^4 - 23 * "x"^3 + 32 * "x"^2 - 19 * "x" + 4

sec5_9_f' :: Polynomial GrLex Rational 3
sec5_9_f' = 24 * "x"^3 - 69 * "x"^2 + 64 * "x"   - 19

sec5_9_gcd :: [Polynomial GrLex Rational 3]
sec5_9_gcd = buchberger [ sec5_9_f, sec5_9_f' ]

sec5_9_div :: [Polynomial GrLex Rational 3]
sec5_9_div = do
  pq0 <- quots $ sec5_9_f /. sec5_9_gcd
  let q0 = quotient pq0
  pq1 <- quots $ q0 /. ["x" - 1]
  [quotient pq1]

sec5_9_check :: Polynomial GrLex Rational 3
sec5_9_check = (6 * "x"^2 - 11 * "x" + 4) * ("x" - 1) * ("x" - 1)

-- PolyQuot PolyQuotsRem (/.)

sec6_1_f :: Polynomial Lex Rational 3
sec6_1_f = "x"^2 * "y" - 3 * "x" * "y"^2 + "x"^2 - 3 * "x" * "y"

sec6_1_g :: Polynomial Lex Rational 3
sec6_1_g = "x"^3 * "y" + "x"^3 - 4 * "y"^2 - 3 * "y" + 1

sec6_1_fx :: Polynomial Lex Rational 3
sec6_1_fx = ("y" + 1) * "x"^2 + (-3 * "y"^2 - 3 * "y") * "x"

sec6_1_gx :: Polynomial Lex Rational 3
sec6_1_gx = ("y" + 1) * "x"^3 - 4 * "y"^2 - 3 * "y" + 1

xa0 :: Polynomial Lex Rational 3
xa0 = "y" + 1

xa1 :: Polynomial Lex Rational 3
xa1 = -3 * "y"^2 - 3 * "y"

xa2 :: Polynomial Lex Rational 3
xa2 = 0

xb0 :: Polynomial Lex Rational 3
xb0 = ("y" + 1)

xb1 :: Polynomial Lex Rational 3
xb1 = 0

xb2 :: Polynomial Lex Rational 3
xb2 = 0

xb3 :: Polynomial Lex Rational 3
xb3 =  - 4 * "y"^2 - 3 * "y" + 1

sec6_1_x_syl :: [[Polynomial Lex Rational 3]]
sec6_1_x_syl =
  [ [ xa0,   0,   0, xb0,   0 ]
  , [ xa1, xa0,   0, xb1, xb0 ]
  , [ xa2, xa1, xa0, xb2, xb1 ]
  , [   0, xa2, xa1, xb3, xb2 ]
  , [   0,  0,  xa2,   0, xb3 ]
  ]

sec6_1_x_res :: Polynomial Lex Rational 3
sec6_1_x_res = det sec6_1_x_syl

-- sec6_1_f :: Polynomial Lex Rational 3
-- sec6_1_f = "x"^2 * "y" - 3 * "x" * "y"^2 + "x"^2 - 3 * "x" * "y"

-- sec6_1_g :: Polynomial Lex Rational 3
-- sec6_1_g = "x"^3 * "y" + "x"^3 - 4 * "y"^2 - 3 * "y" + 1

sec6_1_fy :: Polynomial Lex Rational 3
sec6_1_fy = - 3 * "x" * "y"^2 + ("x"^2 - 3 * "x") * "y" + "x"^2

sec6_1_gy :: Polynomial Lex Rational 3
sec6_1_gy = -4 * "y"^2 + ("x"^3 - 3) * "y" + "x"^3 + 1

ya0 :: Polynomial Lex Rational 3
ya0 = - 3 * "x"

ya1 :: Polynomial Lex Rational 3
ya1 = "x"^2 - 3 * "x"

ya2 :: Polynomial Lex Rational 3
ya2 = "x"^2

yb0 :: Polynomial Lex Rational 3
yb0 = -4

yb1 :: Polynomial Lex Rational 3
yb1 = "x"^3 - 3

yb2 :: Polynomial Lex Rational 3
yb2 = "x"^3 + 1

sec6_1_y_syl :: [[Polynomial Lex Rational 3]]
sec6_1_y_syl =
  [ [ ya0,   0, yb0,   0 ]
  , [ ya1, ya0, yb1, yb0 ]
  , [ ya2, ya1, yb2, yb1 ]
  , [   0, ya2,   0, yb2 ]
  ]

sec6_1_y_res :: Polynomial Lex Rational 3
sec6_1_y_res = det sec6_1_y_syl
