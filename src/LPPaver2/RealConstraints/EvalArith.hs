{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module LPPaver2.RealConstraints.EvalArith
  ( EvalArithmetic (..),
    EvaluatedForm (..),
    SimplifyFormResult (..),
    simplifyEvalForm,
  )
where

import AERN2.MP (MPBall)
import AERN2.MP.Affine (MPAffine)
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Boxes (Box)
import LPPaver2.RealConstraints.Eval
  ( EvaluatedFormR (..),
    OldToNew,
    SimplifyFormResultR (..),
    simplifyEvalFormR,
  )
import LPPaver2.RealConstraints.EvalArithmetic.AffArith ()
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude

data EvalArithmetic
  = EvalArithmeticMPBall {sampleBall :: MPBall}
  | EvalArithmeticAffine {sampleAffine :: MPAffine}

data EvaluatedForm
  = EvaluatedFormMPBall (EvaluatedFormR MPBall)
  | EvaluatedFormAffine (EvaluatedFormR MPAffine)

instance Show EvaluatedForm where
  show _ = "<evaluated form>"

data SimplifyFormResult = SimplifyFormResult
  { evaluatedForm :: EvaluatedForm,
    oldToNew :: OldToNew
  }

simplifyEvalForm ::
  EvalArithmetic ->
  Box ->
  Form ->
  SimplifyFormResult
simplifyEvalForm (EvalArithmeticMPBall {sampleBall}) box formInit =
  let resultR = simplifyEvalFormR sampleBall box formInit
   in SimplifyFormResult
        { evaluatedForm = EvaluatedFormMPBall resultR.evaluatedForm,
          oldToNew = resultR.oldToNew
        }
simplifyEvalForm (EvalArithmeticAffine {sampleAffine}) box formInit =
  let resultR = simplifyEvalFormR sampleAffine box formInit
   in SimplifyFormResult
        { evaluatedForm = EvaluatedFormAffine resultR.evaluatedForm,
          oldToNew = resultR.oldToNew
        }
