module LPPaver2.RealConstraints.EvalSpec (spec) where

import AERN2.Kleenean
import AERN2.MP
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Boxes
import LPPaver2.RealConstraints.Eval
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Expr
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import Test.Hspec

spec :: Spec
spec = describe "simplifyEvalForm" $ do
  it "evaluates trivial constants correctly (True)" $ do
    let box = mkBox []
        sampleR = mpBall (0 :: Integer)
        result = simplifyEvalForm sampleR box formTrue
    getFormDecision result.evaluatedForm.form `shouldBe` CertainTrue

  it "evaluates trivial constants correctly (False)" $ do
    let box = mkBox []
        sampleR = mpBall (0 :: Integer)
        result = simplifyEvalForm sampleR box formFalse
    getFormDecision result.evaluatedForm.form `shouldBe` CertainFalse

  it "evaluates simple comparisons correctly (1 <= 2)" $ do
    let box = mkBox []
        sampleR = mpBall (0 :: Integer)
        form = exprLit (rational 1) <= exprLit (rational 2)
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainTrue

  it "evaluates simple comparisons correctly (2 <= 1)" $ do
    let box = mkBox []
        sampleR = mpBall (0 :: Integer)
        form = exprLit (rational 2) <= exprLit (rational 1)
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainFalse

  it "evaluates expressions with variables correctly (x <= 2 over [0,1])" $ do
    let box = mkBox [("x", (rational 0, rational 1))]
        sampleR = mpBall (0 :: Integer)
        form = exprVar "x" <= exprLit (rational 2)
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainTrue

  it "evaluates expressions with variables correctly (x >= 2 over [0,1])" $ do
    let box = mkBox [("x", (rational 0, rational 1))]
        sampleR = mpBall (0 :: Integer)
        form = exprLit (rational 2) <= exprVar "x"
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainFalse

  it "leaves uncertain forms unresolved (x <= 0.5 over [0,1])" $ do
    let box = mkBox [("x", (rational 0, rational 1))]
        sampleR = mpBall (0 :: Integer)
        form = exprVar "x" <= exprLit (rational (1 / 2))
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` TrueOrFalse

  it "evaluates boolean connectives (True and False)" $ do
    let box = mkBox []
        sampleR = mpBall (0 :: Integer)
        form = formTrue && formFalse
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainFalse

  it "evaluates boolean connectives (not False)" $ do
    let box = mkBox []
        sampleR = mpBall (0 :: Integer)
        form = not formFalse
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainTrue

  it "evaluates boolean connectives (True or (x <= 0.5))" $ do
    let box = mkBox [("x", (rational 0, rational 1))]
        sampleR = mpBall (0 :: Integer)
        form = formTrue || exprVar "x" <= exprLit (rational (1 / 2))
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainTrue

  it "evaluates if-then-else when condition is True" $ do
    let box = mkBox [("x", (rational 0, rational 1))]
        sampleR = mpBall (0 :: Integer)
        form = formIfThenElse formTrue (exprVar "x" <= exprLit (rational 2)) formFalse
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainTrue

  it "evaluates if-then-else when condition is False" $ do
    let box = mkBox [("x", (rational 0, rational 1))]
        sampleR = mpBall (0 :: Integer)
        form = formIfThenElse formFalse formFalse (exprVar "x" <= exprLit (rational 2))
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` CertainTrue

  it "leaves if-then-else uncertain when condition is uncertain" $ do
    let box = mkBox [("x", (rational 0, rational 1))]
        sampleR = mpBall (0 :: Integer)
        form = formIfThenElse (exprVar "x" <= exprLit (rational (1 / 2))) formTrue formFalse
        result = simplifyEvalForm sampleR box form
    getFormDecision result.evaluatedForm.form `shouldBe` TrueOrFalse
