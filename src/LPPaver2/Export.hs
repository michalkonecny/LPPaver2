{-# LANGUAGE OverloadedStrings #-}

module LPPaver2.Export (lppProblemToJSON) where

import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key (fromString)
import Data.Aeson.Types qualified as A
import Data.Map qualified as Map
import GHC.Records (getField)
import LPPaver2.BranchAndPrune (LPPPaving, LPPProblem)
import LPPaver2.RealConstraints
import MixedTypesNumPrelude
import AERN2.Kleenean (Kleenean)

instance A.ToJSON MP.MPBall where
  toJSON b = A.object ["l" .= lD, "u" .= uD]
    where
      (l, u) = MP.endpoints b
      lD = double l
      uD = double u

instance A.ToJSON Box_ where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Box where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Boxes where
  toJSON (Boxes {store}) =
    A.object ["boxes" .= Map.keys store]

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
  toEncoding = A.genericToEncoding (A.defaultOptions {  A.tagSingleConstructors = True })

instance A.ToJSON BinaryConn where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON form) => A.ToJSON (FormF form) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Form where
  toJSON (Form {..}) =
    A.object ["formH" .= root]

instance A.ToJSON (EvaluatedForm r) where
  toJSON (EvaluatedForm {formValues}) =
    A.object
      [ "formValues" .= formValues ]

instance A.ToJSON Kleenean where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON problem, A.ToJSON paving, A.ToJSON evalInfo) => A.ToJSON (BP.Step problem paving evalInfo) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON LPPProblem where
  toJSON = lppProblemToJSON

lppProblemToJSON :: LPPProblem -> A.Value
lppProblemToJSON (BP.Problem {scope, constraint}) =
  A.object ["scope" .= scope.boxHash, "constraint" .= constraint.root]

instance A.ToJSON LPPPaving where
  toJSON = lppPavingToJSON

lppPavingToJSON :: LPPPaving -> A.Value
lppPavingToJSON (BP.Paving {scope, inner, outer, undecided}) =
  A.object
    [ "scope" .= scope.boxHash,
      "inner" .= inner,
      "outer" .= outer,
      "undecided" .= A.listValue lppProblemToJSON undecided
    ]
