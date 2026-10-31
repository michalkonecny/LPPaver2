module LPPaver2.RealConstraints.IntervalAD
  ( IntervalAD (partials),
    evalIntervalAD,
    mpBallBounds,
  )
where

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints.Eval (evalExprWith)
import LPPaver2.RealConstraints.Expr
  ( BinaryOp (..),
    ExprF (..),
    ExprHash,
    ExprStore,
    UnaryOp (..),
    Var,
  )
import LPPaver2.RealConstraints.Expr qualified as Expr
import MixedTypesNumPrelude
import Numeric.CollectErrors qualified as CN

data IntervalAD = IntervalAD
  { value :: MP.MPBall,
    partials :: Map.Map Var MP.MPBall
  }

evalIntervalAD :: ExprStore -> Map.Map ExprHash MP.MPBall -> Map.Map Var MP.MPBall -> ExprHash -> CN IntervalAD
evalIntervalAD exprNodes knownValues domains root =
  Map.findWithDefault (CN.noValueNumErrorCertain (CN.NumError "root expression not evaluated")) root
    $ evalExprWith
      evaluateNode
      Expr.Expr {Expr.nodes = exprNodes, Expr.root = root}
      Map.empty
  where
    literal :: Rational -> MP.MPBall
    literal literalValue = mpBallP precision literalValue
    precision = case Map.elems domains of
      domain : _ -> getPrecision domain
      [] -> MP.defaultPrecision

    evaluateNode exprHash node =
      withKnownValue exprHash <$> calculate node

    calculate node =
      case node of
        ExprVar {var} ->
          case Map.lookup var domains of
            Just domain -> cn IntervalAD {value = domain, partials = Map.singleton var (literal (rational 1))}
            Nothing -> CN.noValueNumErrorCertain (CN.NumError ("no domain for variable " ++ var))
        ExprLit {lit} -> cn IntervalAD {value = literal lit, partials = Map.empty}
        ExprUnary {unop = OpNeg, e1} -> negate e1
        ExprUnary {unop = OpSqrt, e1} -> CN.liftCN sqrt e1
        ExprUnary {unop = OpSin, e1} -> sin e1
        ExprUnary {unop = OpCos, e1} -> cos e1
        ExprBinary {binop = OpPlus, e1, e2} -> e1 + e2
        ExprBinary {binop = OpMinus, e1, e2} -> e1 - e2
        ExprBinary {binop = OpTimes, e1, e2} -> e1 * e2
        ExprBinary {binop = OpDivide, e1, e2} -> CN.lift2CN (/) e1 e2

    withKnownValue exprHash result =
      case Map.lookup exprHash knownValues of
        Just knownValue -> result {value = knownValue}
        Nothing -> result

instance CanNeg IntervalAD where
  negate operand =
    IntervalAD
      { value = negate operand.value,
        partials = Map.map negate operand.partials
      }

instance CanAddAsymmetric IntervalAD IntervalAD where
  add left right =
    IntervalAD
      { value = left.value + right.value,
        partials = Map.unionWith (+) left.partials right.partials
      }

instance CanSub IntervalAD IntervalAD

instance CanMulAsymmetric IntervalAD IntervalAD where
  mul left right =
    IntervalAD
      { value = left.value * right.value,
        partials = Map.unionWith (+) (scalePartials right.value left.partials) (scalePartials left.value right.partials)
      }

instance CanGiveUpIfVeryInaccurate IntervalAD

instance CanDiv IntervalAD IntervalAD where
  type DivType IntervalAD IntervalAD = CN IntervalAD
  divide numerator denominator = do
    reciprocal <- recipByEndpoints denominator.value
    -- MPBall ^ uses endpoints for even powers, so the square stays positive
    inverseSquare <- recipByEndpoints (denominator.value ^ 2)
    pure
      IntervalAD
        { value = numerator.value * reciprocal,
          partials =
            scalePartials
              inverseSquare
              ( Map.unionWith
                  (+)
                  (scalePartials denominator.value numerator.partials)
                  (scalePartials (negate numerator.value) denominator.partials)
              )
        }

-- | The derivative of sqrt is unbounded at 0, so the argument must be certainly positive.
instance CanSqrt IntervalAD where
  type SqrtType IntervalAD = CN IntervalAD
  sqrt operand
    | isCertainlyPositive operand.value = do
        derivativeFactor <- recipByEndpoints (2 * result)
        pure IntervalAD {value = result, partials = scalePartials derivativeFactor operand.partials}
    | otherwise = CN.noValueNumErrorPotential (CN.OutOfDomain "sqrt argument not certainly positive")
    where
      result = sqrt operand.value

instance CanSinCos IntervalAD where
  sin operand =
    IntervalAD {value = sin operand.value, partials = scalePartials (cos operand.value) operand.partials}
  cos operand =
    IntervalAD {value = cos operand.value, partials = scalePartials (negate (sin operand.value)) operand.partials}

scalePartials :: MP.MPBall -> Map.Map Var MP.MPBall -> Map.Map Var MP.MPBall
scalePartials factor = Map.map (factor *)

mpBallBounds :: MP.MPBall -> (Rational, Rational)
mpBallBounds ball = (rational lower, rational upper)
  where
    (lower, upper) = MP.endpoints ball

-- | MPBall division widens the result by a factor of about 1 + radius/|centre|,
-- losing the sign once the radius exceeds half the centre.  Taking reciprocals
-- of the endpoints only loses accuracy to rounding.
recipByEndpoints :: MP.MPBall -> CN MP.MPBall
recipByEndpoints ball
  | isCertainlyNonZero ball = cn (MP.fromEndpointsAsIntervals (recip upper) (recip lower))
  | otherwise = recip (cn ball) -- reports the division by zero
  where
    lower = MP.endpointLAsInterval ball
    upper = MP.endpointRAsInterval ball
