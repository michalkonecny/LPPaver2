{-# LANGUAGE OverloadedStrings #-}

module LPPaver2.Export () where

import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key (fromString)
import GHC.Records ()
import LPPaver2.RealConstraints
import MixedTypesNumPrelude

instance A.ToJSON MP.MPBall where
  toJSON b = A.object ["l" .= lD, "u" .= uD]
    where
      (l, u) = MP.endpoints b
      lD = double l
      uD = double u

instance A.ToJSON Box where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Boxes where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON UnaryOp where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON BinaryOp where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON expr) => A.ToJSON (ExprF expr) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Expr where
  toJSON (Expr {..}) =
    A.object ["exprH" .= root]

instance A.ToJSON BinaryComp where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON UnaryConn where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON BinaryConn where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON form) => A.ToJSON (FormF form) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Form where
  toJSON (Form {..}) =
    A.object ["formH" .= root]

instance (A.ToJSON problem, A.ToJSON paving) => A.ToJSON (BP.Step problem paving) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON constraint, A.ToJSON scope) => A.ToJSON (BP.Problem constraint scope) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON constraint, A.ToJSON basicSet, A.ToJSON set) => A.ToJSON (BP.Paving constraint basicSet set) where
  toEncoding = A.genericToEncoding A.defaultOptions