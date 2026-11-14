module LPPaver2.RealConstraints.BoxesSpec (spec) where

import Data.Map qualified as Map
import LPPaver2.RealConstraints.Boxes
import MixedTypesNumPrelude
import Test.Hspec

spec :: Spec
spec = describe "boxAreaD" $ do
  -- boxAreaD multiplies half-widths, so the area of this box is 2 * 1
  let box = mkBox [("x", (0.0, 4.0)), ("y", (0.0, 2.0))]
      withParam = addParamValuesToBox (Map.fromList [("p", 1.0)])

  it "multiplies the half-widths of the volume variables" $ do
    boxAreaD box `shouldBe` double 2.0

  it "ignores variables that are not volume variables" $ do
    boxAreaD (withParam box) `shouldBe` double 2.0

  it "subtracts the excluded region" $ do
    let remaining = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 2.0))]
    boxAreaD (mkBoxDifference box remaining) `shouldBe` double 1.5

  it "subtracts the excluded region when it also lists a zero-width parameter" $ do
    let remaining = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 2.0))]
    boxAreaD (mkBoxDifference (withParam box) (withParam remaining)) `shouldBe` double 1.5

  it "counts only the part of the excluded region that is inside the box" $ do
    let sticksOut = mkBox [("x", (3.0, 6.0)), ("y", (-1.0, 2.0))]
    boxAreaD (mkBoxDifference box sticksOut) `shouldBe` double 1.5
