module LPPaver2.RealConstraints.Subst (
  determineVarsInForm,
) where

import Data.Map qualified as Map
import GHC.Records (HasField (..))
import LPPaver2.RealConstraints.Expr (Expr (..), ExprF (..), ExprHash, expr1, expr2, exprLit, exprVar)
import LPPaver2.RealConstraints.Form
  ( Form (..),
    FormF (..),
    FormHash,
    formComp,
    lookupFormExprNode,
    lookupFormNode, form1, form2, formTrue, formFalse, formIfThenElse,
  )
import MixedTypesNumPrelude

determineVarsInForm :: Form -> Map.Map String Rational -> Form
determineVarsInForm form varValuation =
  determineVarsInFormNode rootNode
  where
    rootNode = lookupFormNode form form.root
    
    determineVarsInFormNode :: FormF FormHash -> Form
    determineVarsInFormNode formNode =
      case formNode of
        FormComp {comp, e1, e2} ->
          let newE1 = determineVarsInExprHash e1
              newE2 = determineVarsInExprHash e2
           in formComp comp newE1 newE2
        FormUnary {uconn, f1} ->
          let newF1 = determineVarsInFormNode (lookupFormNode form f1)
           in form1 uconn newF1
        FormBinary {bconn, f1, f2} ->
          let newF1 = determineVarsInFormNode (lookupFormNode form f1)
              newF2 = determineVarsInFormNode (lookupFormNode form f2)
           in form2 bconn newF1 newF2
        FormIfThenElse {fc, ft, ff} ->
          let newFc = determineVarsInFormNode (lookupFormNode form fc)
              newFt = determineVarsInFormNode (lookupFormNode form ft)
              newFf = determineVarsInFormNode (lookupFormNode form ff)
           in formIfThenElse newFc newFt newFf
        FormTrue -> formTrue
        FormFalse -> formFalse

    determineVarsInExprHash :: ExprHash -> Expr
    determineVarsInExprHash exprHash =
      case lookupFormExprNode form exprHash of
        ExprVar varName ->
          case Map.lookup varName varValuation of
            Just q -> exprLit q
            Nothing -> exprVar varName
        ExprLit q -> exprLit q
        ExprUnary {unop, e1} ->
          let newE1 = determineVarsInExprHash e1
           in expr1 unop newE1
        ExprBinary {binop, e1, e2} ->
          let newE1 = determineVarsInExprHash e1
              newE2 = determineVarsInExprHash e2
           in expr2 binop newE1 newE2
