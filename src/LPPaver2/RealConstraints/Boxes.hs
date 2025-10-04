{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}

module LPPaver2.RealConstraints.Boxes
  ( --
    Box (..),
    mkBox,
    boxAreaD,
    splitBox,
    BoxHash,
    BoxStore,
    BoxesList (..),
    Boxes (..),
    boxesCount,
    boxesAreaD,
  )
where

import AERN2.MP (MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Affine.Type ()
import AERN2.MP.Ball (CentreRadius (CentreRadius))
import AERN2.MP.Ball.Type qualified as MP
import AERN2.MP.Dyadic (dyadic)
-- has instance Hashable MPBall (TODO: move that instance)

import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.RealConstraints.Expr (Var)
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

{- N-dimensional Boxes -}

data Box = Box {varDomains :: Map.Map Var MP.MPBall, splitOrder :: [Var]}
  deriving (Generic, Hashable)

instance Show Box where
  show (Box {..}) =
    printf "[%s]" $ List.intercalate ", " $ map showVarDom $ Map.toList varDomains
    where
      showVarDom :: (Var, MPBall) -> String
      showVarDom (var, ball) = printf "%s ∈ [%s..%s]" var (show (double l)) (show (double u))
        where
          (l, u) = MP.endpoints ball

instance P.Eq Box where
  b1 == b2 = varDomainsR b1 == varDomainsR b2

varDomainsR :: Box -> [(Var, (Rational, Rational))]
varDomainsR b = sortOn fst $ map fromBall (Map.toList b.varDomains)
  where
    fromBall (var, ball) = (var, (rational lR, rational uR))
      where
        (lR, uR) = MP.endpoints ball

instance MP.HasPrecision Box where
  getPrecision (Box {}) = MP.defaultPrecision -- TODO : use precision from varDomains if possible

mkBox :: [(Var, (Rational, Rational))] -> Box
mkBox varDomainsRational =
  Box
    { varDomains = Map.fromList (map toBall varDomainsRational),
      splitOrder = map fst varDomainsRational
    }
  where
    toBall (var, (lR, uR)) = (var, MP.mpBall (CentreRadius mR rR))
      where
        mR = (lR + uR) / 2
        rR = (uR - lR) / 2

boxAreaD :: Box -> Double
boxAreaD (Box {..}) =
  product (map (double . dyadic . MP.radius) (Map.elems varDomains))

{- Collections of boxes. -}

type BoxHash = Int

type BoxStore = Map.Map BoxHash Box

boxHAreaD :: BoxStore -> BoxHash -> Double
boxHAreaD store boxH = case Map.lookup boxH store of
  Nothing -> double 0 -- Box not found
  Just box -> boxAreaD box

data BoxesList
  = BoxesList [BoxHash]
  | BoxesUnion [BoxesList]
  deriving (P.Eq, Generic)

data Boxes = Boxes {store :: BoxStore, list :: BoxesList}
  deriving (Generic)

instance P.Show Boxes where
  show (Boxes {store, list}) =
    printf
      "Boxes(count=%d, area=%.3f)"
      (boxesCount (Boxes {store, list}))
      (boxesAreaD (Boxes {store, list}))

boxesCount :: Boxes -> Integer
boxesCount (Boxes {list}) = boxesListCount list
  where
    boxesListCount (BoxesList boxes) = length boxes
    boxesListCount (BoxesUnion union) = sum (map boxesListCount union)

boxesAreaD :: Boxes -> Double
boxesAreaD (Boxes {store, list}) = boxesListAreaD list
  where
    boxesListAreaD (BoxesList boxes) = sum (map (boxHAreaD store) boxes)
    boxesListAreaD (BoxesUnion union) = sum (map boxesListAreaD union)

splitBox :: Box -> [Box]
splitBox box = case box.splitOrder of
  [] -> [box] -- No split order?? Perhaps there are no variables that can be split, ie are exact points.
  (splitVar : splitRest) ->
    -- We split the first variable in the list.
    let splitOrder = splitRest ++ [splitVar] -- Cycle the variables round-robin-like.
     in case Map.lookup splitVar box.varDomains of
          Nothing -> [box] -- The split variable does not exist...
          Just splitVarDomain ->
            [ Box {varDomains = varDomainsL, splitOrder},
              Box {varDomains = varDomainsU, splitOrder}
            ]
            where
              (splitVarDomainL, splitVarDomainU) = splitMPBall splitVarDomain
              varDomainsL = Map.insert splitVar splitVarDomainL box.varDomains
              varDomainsU = Map.insert splitVar splitVarDomainU box.varDomains

splitMPBall :: MPBall -> (MPBall, MPBall)
splitMPBall b = (bL, bU)
  where
    (l, u) = MP.mpBallEndpoints b
    m = (l + u) / 2 -- TODO: adjust precision if needed to get the exact middle
    bL = MP.fromMPBallEndpoints l m
    bU = MP.fromMPBallEndpoints m u
