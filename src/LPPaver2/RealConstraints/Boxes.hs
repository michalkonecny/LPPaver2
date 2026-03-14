{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}

module LPPaver2.RealConstraints.Boxes
  ( --
    Box (..),
    Box_ (..),
    mkBox,
    boxAreaD,
    splitBox,
    BoxHash (..),
    BoxStore,
    Boxes (..),
    boxesCount,
    boxesAreaD,
    boxRestrictSplitOrder,
  )
where

import AERN2.MP (MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Affine.Type ()
import AERN2.MP.Ball (CentreRadius (CentreRadius))
import AERN2.MP.Ball.Type qualified as MP
import AERN2.MP.Dyadic (dyadic)
-- has instance Hashable MPBall (TODO: move that instance)

import Data.Hashable (Hashable (hash))
import Data.List (sortOn)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.RealConstraints.Expr (Var)
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

{- N-dimensional Boxes -}

type VarDomains = Map.Map Var MPBall

data Box_
  = Box_
  { varDomains :: VarDomains,
    splitOrder :: [Var],
    -- | If present, the box is the set of points in varDomains that are not in except.
    -- ^ This is used to represent the result of pruning a box,
    -- ^ resulting in another box + the difference between the original and the pruned box.
    except :: Maybe VarDomains
  }
  deriving (Generic, Hashable)

instance P.Eq Box_ where
  b1 == b2 = varDomainsR b1 == varDomainsR b2

varDomainsR :: Box_ -> [(Var, (Rational, Rational))]
varDomainsR b = sortOn fst $ map fromBall (Map.toList b.varDomains)
  where
    fromBall (var, ball) = (var, (rational lR, rational uR))
      where
        (lR, uR) = MP.endpoints ball

data Box = Box {boxHash :: BoxHash, box_ :: Box_}
  deriving (Generic)

boxWithHash :: Box_ -> Box
boxWithHash box_ = Box {boxHash = BoxHash (hash box_), box_}

instance Show Box where
  show (Box {box_ = Box_ {varDomains, except}}) =
    case except of
      Nothing -> printf "[%s]" $ showVarDomains varDomains
      Just exceptVarDomains ->
        printf "[%s] \\ [%s]" (showVarDomains varDomains) (showVarDomains exceptVarDomains)
    where
      showVarDomains :: VarDomains -> String
      showVarDomains = List.intercalate ", " . map showVarDom . Map.toList

      showVarDom :: (Var, MPBall) -> String
      showVarDom (var, ball) = printf "%s ∈ [%s..%s]" var (show (double l)) (show (double u))
        where
          (l, u) = MP.endpoints ball

instance MP.HasPrecision Box where
  getPrecision (Box {}) = MP.defaultPrecision -- TODO : use precision from varDomains if possible

mkBox :: [(Var, (Rational, Rational))] -> Box
mkBox varDomainsRational =
  Box {boxHash = BoxHash (hash box_), box_}
  where
    box_ =
      Box_
        { varDomains = Map.fromList (map toBall varDomainsRational),
          splitOrder = map fst varDomainsRational,
          except = Nothing
        }
    toBall (var, (lR, uR)) = (var, MP.mpBall (CentreRadius mR rR))
      where
        mR = (lR + uR) / 2
        rR = (uR - lR) / 2

boxAreaD :: Box -> Double
boxAreaD box =
  product
    ( map
        (double . dyadic . MP.radius)
        (Map.elems box.box_.varDomains)
    )

{- Collections of boxes. -}

newtype BoxHash = BoxHash {unBoxHash :: Int}
  deriving (P.Eq, P.Ord, Generic)

instance Hashable BoxHash where
  hash (BoxHash h) = hash h

type BoxStore = Map.Map BoxHash Box

newtype Boxes = Boxes {store :: BoxStore}
  deriving (Generic)

instance P.Show Boxes where
  show boxes =
    printf
      "Boxes(count=%d, area=%.3f)"
      (boxesCount boxes)
      (boxesAreaD boxes)

boxesCount :: Boxes -> Integer
boxesCount (Boxes {store}) = integer $ Map.size store

boxesAreaD :: Boxes -> Double
boxesAreaD (Boxes {store}) = sum (map boxAreaD (Map.elems store))

{-- Changing the splitting order --}

-- |
-- Restrict the splitting order of a box to only the given variables.
--
-- Useful when some variables do not appear in the constraints anymore.
boxRestrictSplitOrder :: Set.Set Var -> Box -> Box
boxRestrictSplitOrder vars box =
  boxWithHash $ box.box_ {splitOrder = filter (`Set.member` vars) box.box_.splitOrder}

{-- Splitting a box --}

splitBox :: Box -> [Box]
splitBox box = case box.box_.splitOrder of
  [] -> error "Internal error: Attempt to split a box with no split order."
  (splitVar : splitRest) ->
    -- We split the first variable in the list.
    let splitOrder = splitRest ++ [splitVar] -- Cycle the variables round-robin-like.
        varDomains = box.box_.varDomains
     in case box.box_.except of
          Just _ ->
            -- This should not happen since except regions are used only for decided regions.
            error "Internal error: Cannot split a box with an except region."
          _ ->
            case Map.lookup splitVar varDomains of
              Nothing -> error "Internal error: The split variable does not exist."
              Just splitVarDomain ->
                [ boxWithHash $ Box_ {varDomains = varDomainsL, splitOrder, except = Nothing},
                  boxWithHash $ Box_ {varDomains = varDomainsU, splitOrder, except = Nothing}
                ]
                where
                  (splitVarDomainL, splitVarDomainU) = splitMPBall splitVarDomain
                  varDomainsL = Map.insert splitVar splitVarDomainL varDomains
                  varDomainsU = Map.insert splitVar splitVarDomainU varDomains

splitMPBall :: MPBall -> (MPBall, MPBall)
splitMPBall b = (bL, bU)
  where
    (l, u) = MP.mpBallEndpoints b
    m = (l + u) / 2 -- TODO: adjust precision if needed to get the exact middle
    bL = MP.fromMPBallEndpoints l m
    bU = MP.fromMPBallEndpoints m u
