{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Polynomial.Data
       ( Mono, degrees, primeMono, monoSing, monoCompare
       , Term, coeff, mono, term, totalDeg, coeffMult

       , Var, varNum
       , Variables, variables, varc, fieldVariables', fieldVariables

       , Polynomial, GPolynomial, terms, polynomial
       , polyPlus, polyMult, polySubt, polyNegate
       , Polynomial1
       , leadingTerm, leadingMono, multiDegree

       , polyQuotRem, PolyQuots
       ) where

import GHC.TypeLits (Nat, Sing, SingI, SingE, sing, fromSing)
import Control.Arrow (second)
import Control.Applicative ((<$>), pure, (<|>))
import Control.Monad (msum)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State.Strict (State, state, execState)
import Data.Monoid (Monoid(..), (<>))
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (foldl', sortBy, groupBy)
import Data.Map.Strict (Map, insertWith)
import qualified Data.Map.Strict as Map
import Data.DList (DList)
import qualified Data.DList as DList
import Data.String (IsString (..))

import Math.Polynomial.Degree
  (Degrees, primeDegrees, degreeSum, degreeSubt)
import Math.Polynomial.Ord
  (invCompare, chainOrdering,
   DegOrder2, degCompare, DegreeOrder (..),
   GrevLex)

fromIntegralSing :: Integral a => Sing (n :: Nat) -> a
fromIntegralSing =  fromInteger . fromSing


newtype Mono k (n :: Nat) = Mono { degrees :: Degrees n }
                            deriving (Eq, Ord, Monoid, Show, Read)

monoCompare :: DegOrder2 o n -> Mono k n -> Mono k n -> Ordering
monoCompare o = degCompare o `on` degrees

monoSing :: SingI n => Mono k n -> Sing n
monoSing =  const sing

primeMono :: SingI n => [Int] -> Mono k n
primeMono =  Mono . primeDegrees

monoDiv :: Mono k n -> Mono k n -> Maybe (Mono k n)
monoDiv x y = Mono <$> (degreeSubt `on` degrees) x y


data Term k (n :: Nat) =
  Term
  { coeff :: !k
  , mono  :: !(Mono k n)
  } deriving (Eq, Show, Read)

instance Ord k => Ord (Term k n) where
  Term c0 m0 `compare` Term c1 m1 = (m0, c0) `compare` (m1, c1)

instance (Num k, SingI n) => Monoid (Term k n) where
  mempty   = Term { coeff = 1, mono = mempty }
  Term { coeff = xc, mono = xm } `mappend` Term { coeff = yc, mono = ym } =
    Term { coeff = xc * yc, mono = xm <> ym }

term :: SingI n => k -> [Int] -> Term k n
term ce = Term ce . primeMono

termCompare :: Ord k => DegOrder2 o n -> Term k n -> Term k n -> Ordering
termCompare o (Term c0 m0) (Term c1 m1) =
  chainOrdering (monoCompare o m0 m1) (compare c0 c1)

termListCompare :: (Num k, Ord k, SingI n)
             => DegOrder2 o n
             -> [Term k n]
             -> [Term k n]
             -> Ordering
termListCompare o xs ys = xs `comp` ys  where
    sz = length xs `max` length ys
    es = repeat mempty
    p `comp` q = foldr chainOrdering EQ . take sz
                 $ zipWith (termCompare o) (p ++ es) (q ++ es)

coeffMult :: Num k => k -> Term k n -> Term k n
coeffMult x t = t { coeff = x * coeff t }

totalDeg :: Term k n -> Int
totalDeg =  degreeSum . degrees . mono

termNegate :: Num k => Term k n -> Term k n
termNegate t = t { coeff = - coeff t }

termDiv :: Fractional k => Term k n -> Term k n -> Maybe (Term k n)
termDiv x y =  Term (coeff x / coeff y) <$> (monoDiv `on` mono) x y

-- termSing :: SingI n => Term k n -> Sing n
-- termSing =  const sing

sortTerms :: Ord k => DegOrder2 o n -> [Term k n] -> [Term k n]
sortTerms =  sortBy . invCompare . termCompare

mergeTerms :: Ord k => DegOrder2 o n -> [Term k n] -> [Term k n] -> [Term k n]
mergeTerms o = rec' where
  rec' xs          []         = xs
  rec' []          ys@(_:_)   = ys
  rec' xxs@(x:xs)  yys@(y:ys)
    | comp == GT || comp == EQ  =  x : rec' xs yys
    | otherwise                 =  y : rec' xxs ys
    where comp = termCompare o x y

mergeTermLists :: Ord k => DegOrder2 o n -> [[Term k n]] -> [Term k n]
mergeTermLists o = rec1  where
  rec0 []       = []
  rec0 [x]      = [x]
  rec0 (x:y:xs) = mergeTerms o x y : rec0 xs
  rec1 []          = []
  rec1 [x]         = x
  rec1 xs@(_:_:_)  = rec1 $ rec0 xs


newtype Var k (n :: Nat) = Var Int deriving (Eq, Ord)

varNum :: Var k n -> Int
varNum (Var i) = i

varSing :: SingI n => Var k n -> Sing n
varSing =  const sing

firstVar :: Sing (n :: Nat) -> Int
firstVar s = fromEnum 'z' + 1 - fromIntegralSing s

instance SingI n => Show (Var k n) where
  show v@(Var i) = [toEnum (fv + i)]  where
    fv = firstVar $ varSing v

readsPrecWithSing :: Sing n -> Int -> ReadS (Var k n)
readsPrecWithSing sg _ = f  where
  f in' = [ (Var $ fromEnum c - fv, cs)
          | ([c], cs) <- lex in'
          , let fv = firstVar sg
          , toEnum fv <= c, c <= 'z' ]

instance SingI n => Read (Var k n) where
  readsPrec = readsPrecWithSing sing

instance SingI n => IsString (Var k n) where
  fromString s = case [v | (v, "") <- reads s ] of
    []     ->  error $ "Unknown variable: " ++ s
    v:_    ->  v

data Variables k (n :: Nat) =
  Variables { varc :: Int }

fieldVariables' :: Sing n -> Variables k n
fieldVariables' =  Variables . fromIntegralSing

fieldVariables :: SingI n => Variables k n
fieldVariables =  fieldVariables' sing

variables' :: Variables k n -> [Var k n]
variables' vf =  [ Var i | i <- [0 .. varc vf - 1] ]

variables :: Sing n -> [Var k n]
variables =  variables' . fieldVariables'

newtype Polynomial o k (n :: Nat) =
  Polynomial { terms :: [Term k n] } deriving (Eq, Show, Read)

-- type propagate hack
polyOrder :: DegreeOrder o => Polynomial o k n -> DegOrder2 o n
polyOrder =  const degreeOrder

instance (SingI n, Ord k, Num k, DegreeOrder o) => Ord (Polynomial o k n)  where
  x `compare` y = terms x `comp` terms y  where
    comp = termListCompare (polyOrder x)

type GPolynomial = Polynomial GrevLex

polyAggregateTerms :: (Eq k, Num k) => [Term k n] -> Polynomial o k n
polyAggregateTerms =  Polynomial . nzero . map usum . grp  where
  grp  =  groupBy ((==) `on` mono)
  usum  []    = error "unsafely sum terms: Broken group passed!"
  usum (t:ts) = foldl'
                (\ta t' -> ta { coeff = coeff ta + coeff t' })
                t ts
  nzero ts = [t | t <- ts, coeff t /= 0 ]

polySortTerms' :: (Ord k, Num k)
               => DegOrder2 o n
               -> [Term k n]
               -> Polynomial o k n
polySortTerms' o = polyAggregateTerms . sortTerms o

polySortTerms :: (Ord k, Num k, DegreeOrder o) => [Term k n] -> Polynomial o k n
polySortTerms =  polySortTerms' degreeOrder

polyMergeTerms :: (Ord k, Num k)
               => DegOrder2 o n
               -> [Term k n]
               -> [Term k n]
               -> Polynomial o k n
polyMergeTerms o a b = polyAggregateTerms $ mergeTerms o a b

polyMergeTermLists :: (Ord k, Num k)
                   => DegOrder2 o n
                   -> [[Term k n]]
                   -> Polynomial o k n
polyMergeTermLists o =  polyAggregateTerms . mergeTermLists o

polynomial :: (Ord k, Num k, DegreeOrder o) => [Term k n] -> Polynomial o k n
polynomial =  polySortTerms

polyPlus :: (Ord k, Num k, DegreeOrder o)
         => Polynomial o k n
         -> Polynomial o k n
         -> Polynomial o k n
p0 `polyPlus` p1 = polyMergeTerms degreeOrder (terms p0) (terms p1)

polyMult :: (SingI n, Ord k, Num k, DegreeOrder o)
         => Polynomial o k n
         -> Polynomial o k n
         -> Polynomial o k n
p0 `polyMult` p1 =
  polyMergeTermLists degreeOrder
  [ [ x <> y | x <- terms p0 ]
  | y <- terms p1
  ]

mapPoly :: (Term k n -> Term k' n') -> Polynomial o k n -> Polynomial o k' n'
mapPoly f p = p { terms = [ f t | t <- terms p ] }

polyNegate :: Num k => Polynomial o k n -> Polynomial o k n
polyNegate =  mapPoly termNegate

polySubt :: (Ord k, Num k, DegreeOrder o)
         => Polynomial o k n
         -> Polynomial o k n
         -> Polynomial o k n
p0 `polySubt` p1 = p0 `polyPlus` polyNegate p1

instance (SingI n, Ord k, Num k, DegreeOrder o) => Num (Polynomial o k n)  where
  (+) = polyPlus
  (*) = polyMult
  (-) = polySubt
  negate = polyNegate
  fromInteger i
    | i == 0    =  polynomial []
    | otherwise =  polynomial [ mempty { coeff = fromInteger i } ]

  abs = id
  signum _ = 1

polyUncons :: Polynomial o k n -> Maybe (Term k n, Polynomial o k n)
polyUncons = d . terms where
  d []      =  Nothing
  d (t:ts)  =  Just (t, Polynomial ts)


leadingTerm :: Polynomial o k n -> Maybe (Term k n)
leadingTerm =  (fst <$>) . polyUncons

leadingMono :: Polynomial o k n -> Maybe (Mono k n)
leadingMono =  (mono <$>) . leadingTerm

multiDegree :: Polynomial o k n -> Maybe (Degrees n)
multiDegree =  (degrees <$>) . leadingMono


type DTerms k n = DList (Term k n)

data DivisionContext o k n =
  DivisionContext
  { divisee   :: !(Polynomial o k n)
  , quotient  :: !(Map (Polynomial o k n) (DTerms k n))
  , remainder :: !(DTerms k n)
  }

finalizeDTerms :: DTerms k n -> Polynomial o k n
finalizeDTerms =  Polynomial . DList.toList

type PolynomialDivision o k n = MaybeT (State (DivisionContext o k n))

polynomialDivision :: (DivisionContext o k n -> (Maybe a, DivisionContext o k n))
                   -> PolynomialDivision o k n a
polynomialDivision =  MaybeT . state

runPolynomialDivision :: Polynomial o k n
                      -> PolynomialDivision o k n a
                      -> ([(Polynomial o k n, Polynomial o k n)], Polynomial o k n)
runPolynomialDivision f = result . (`execState` is) . runMaybeT  where
  is = DivisionContext { divisee = f, quotient = Map.empty, remainder = mempty }
  result c =
    (map (second finalizeDTerms) . reverse . Map.toList $ quotient c,
     finalizeDTerms $ remainder c)

pushRemainder' :: DivisionContext o k n -> (Maybe (), DivisionContext o k n)
pushRemainder' c = case polyUncons $ divisee c of
  Nothing       -> (Nothing, c)
  Just (lt, p') -> (Just (), c { divisee = p', remainder = remainder c <> pure lt })

pushRemainder :: PolynomialDivision o k n ()
pushRemainder =  polynomialDivision pushRemainder'

applyDivisor' :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => (Term k n, Polynomial o k n)
             -> DivisionContext o k n
             -> (Maybe (), DivisionContext o k n)
applyDivisor' (ltF', f') c = mayStep $ \(q, p) ->
  (Just (), c { divisee = p, quotient = insertWith (flip (<>)) f' (pure q) (quotient c) })
  where
    mayStep f = maybe (Nothing, c) f $ do
      let p = divisee c
      lt <- leadingTerm p
      q  <- lt `termDiv` ltF'
      return (q, p - mapPoly (q <>) f')

applyDivisor :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => (Term k n, Polynomial o k n)
             -> PolynomialDivision o k n ()
applyDivisor =  polynomialDivision . applyDivisor'

divisionLoop :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => [Polynomial o k n]
             -> PolynomialDivision o k n ()
divisionLoop dps' = rec'  where
  dps = prepareDivisors dps'
  rec' =
    do msum $ map applyDivisor dps
       rec'
    <|>
    do pushRemainder
       rec'
    <|>
       return ()

prepareDivisors :: (Num k, Ord k, SingI n, DegreeOrder o)
                => [Polynomial o k n]
                -> [(Term k n, Polynomial o k n)]
prepareDivisors ds' = dps  where
  ds =  sortBy (invCompare compare) $ filter (/= 0) ds'
  lts = fromMaybe (error "Bug?: leading terms") $ mapM leadingTerm ds
  dps = zip lts ds

type PolyQuots o k n = [(Polynomial o k n, Polynomial o k n)]

polyQuotRem :: (Fractional k, Ord k, SingI n, DegreeOrder o)
            => Polynomial o k n
            -> [Polynomial o k n]
            -> (PolyQuots o k n, Polynomial o k n)
polyQuotRem f ds =
  runPolynomialDivision f $ divisionLoop ds

type Polynomial1 o k = Polynomial o k 1
