{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Math.Polynomial.Algorithm.Division
       ( PolyQuot (..), PolyQuotsRem (..), polyQuotRem
       ) where

import GHC.TypeLits (SingI)
import Control.Applicative (Applicative, (<$>), pure, (<|>))
import Control.Monad (msum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State.Strict (State, get, modify, execState)
import Data.Monoid (Monoid(..), (<>))
import Data.Maybe (mapMaybe)
import Data.IntMap.Strict (IntMap, insertWith)
import qualified Data.IntMap.Strict as Map
import Data.DList (DList)
import qualified Data.DList as DList

import Math.Polynomial.Ord (DegreeOrder (..))
import Math.Polynomial.Data
  (Term, termDiv,
   Polynomial, polyUncons, leadingTerm, unsafePolynomial, unsafeMapPoly)


type DTerms k n = DList (Term k n)

data DivisionContext o k n =
  DivisionContext
  { contDivisee   :: !(Polynomial o k n)
  , contQuotient  :: !(IntMap (Polynomial o k n, DTerms k n))
  , contRemainder :: !(DTerms k n)
  }

finalizeDTerms :: DTerms k n -> Polynomial o k n
finalizeDTerms =  unsafePolynomial . DList.toList

type PolynomialDivision o k n = MaybeT (State (DivisionContext o k n))

hoist :: Maybe a -> PolynomialDivision o k n a
hoist =  MaybeT . return

data PolyQuot o k n =
  PolyQuot
  { quotient :: Polynomial o k n
  , divisor  :: Polynomial o k n
  } deriving (Eq, Ord, Show)

data PolyQuotsRem o k n =
  PolyQuotsRem
  { quots     :: [PolyQuot o k n]
  , remainder :: Polynomial o k n
  } deriving (Eq, Ord, Show)

runPolynomialDivision :: Polynomial o k n
                      -> PolynomialDivision o k n a
                      -> PolyQuotsRem o k n
runPolynomialDivision f = result . (`execState` is) . runMaybeT  where
  is = DivisionContext { contDivisee = f, contQuotient = Map.empty, contRemainder = mempty }
  result c =
    (PolyQuotsRem
     { quots = [ PolyQuot { quotient  =  finalizeDTerms q
                          , divisor   =  d
                          }
               | (_ix, (d, q)) <- Map.toList $ contQuotient c ]
     , remainder = finalizeDTerms $ contRemainder c
     })

pushRemainder :: PolynomialDivision o k n ()
pushRemainder = do
  p         <-  contDivisee <$> lift get
  (lt, p')  <-  hoist $ polyUncons p
  lift $ modify (\c -> c { contDivisee = p', contRemainder = contRemainder c <> pure lt } )

type Divisor o k n = (Term k n, (Int, Polynomial o k n))

applyDivisor :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => Divisor o k n
             -> PolynomialDivision o k n ()
applyDivisor (ltF', (ix, f')) = do
  p  <-  contDivisee <$> lift get
  q  <-  hoist $ do
    lt <- leadingTerm p
    lt `termDiv` ltF'
  let plus (_, qn) (f, qo) = (f, qo <> qn)
      appendQ = insertWith plus ix (f', pure q)
  lift $ modify (\c -> c { contDivisee   =  p - unsafeMapPoly (q <>) f'
                         , contQuotient  =  appendQ $ contQuotient c
                         })

divisionLoop :: (Fractional k, Ord k, SingI n, DegreeOrder o)
             => [Divisor o k n]
             -> PolynomialDivision o k n ()
divisionLoop dps = rec'  where
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
                -> [Divisor o k n]
prepareDivisors ds = dps  where
  tryD p@(_, d) = do
    lt <- leadingTerm d
    return (lt, p)
  dps = mapMaybe tryD $ zip [0..] ds

polyQuotRem :: (Fractional k, Ord k, SingI n, DegreeOrder o)
            => Polynomial o k n
            -> [Polynomial o k n]
            -> PolyQuotsRem o k n
polyQuotRem f dps =
  runPolynomialDivision f . divisionLoop $ prepareDivisors dps
