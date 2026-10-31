module LPPaver2.Linearisation
  ( LinearRelaxation (..),
    RelaxComparison,
    relaxComparisonWith,
    exactRelaxation,
    affineRelaxations,
    adCornerRelaxations,
  )
where

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
import AERN2.MP.Affine (ErrorTermId, MPAffine (..))
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import GHC.Records
import LPPaver2.RealConstraints.Boxes
  ( Box (box_),
    Box_ (varDomains),
    boxWithHash,
  )
import LPPaver2.RealConstraints.Eval (evalExpr, evalExprWith)
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Expr
  ( BinaryOp (..),
    ExprF (..),
    ExprHash,
    ExprStore,
    UnaryOp (..),
    Var,
  )
import LPPaver2.RealConstraints.Expr qualified as Expr
import LPPaver2.RealConstraints.IntervalAD
  ( IntervalAD (partials),
    evalIntervalAD,
    mpBallBounds,
    subtractIntervalAD,
  )
import MixedTypesNumPrelude
import Prelude qualified as P

data AffineExpression = AffineExpression
  { expressionCoefficients :: Map.Map Var Rational,
    expressionConstant :: Rational
  }

data LinearRelaxation = LinearRelaxation
  { coefficients :: Map.Map Var Rational,
    rhs :: Rational
  }

zeroExpression :: AffineExpression
zeroExpression = AffineExpression {expressionCoefficients = Map.empty, expressionConstant = rational 0}

linearVar :: Var -> AffineExpression
linearVar var = zeroExpression {expressionCoefficients = Map.singleton var (rational 1)}

linearConst :: Rational -> AffineExpression
linearConst value = zeroExpression {expressionConstant = value}

negateExpression :: AffineExpression -> AffineExpression
negateExpression expression =
  AffineExpression
    { expressionCoefficients = Map.map P.negate expression.expressionCoefficients,
      expressionConstant = P.negate expression.expressionConstant
    }

addExpressions :: AffineExpression -> AffineExpression -> AffineExpression
addExpressions expression1 expression2 =
  AffineExpression
    { expressionCoefficients = Map.unionWith (P.+) expression1.expressionCoefficients expression2.expressionCoefficients,
      expressionConstant = expression1.expressionConstant P.+ expression2.expressionConstant
    }

subtractExpressions :: AffineExpression -> AffineExpression -> AffineExpression
subtractExpressions expression1 expression2 = addExpressions expression1 (negateExpression expression2)

scaleExpression :: Rational -> AffineExpression -> AffineExpression
scaleExpression scale expression =
  AffineExpression
    { expressionCoefficients = Map.map (P.* scale) expression.expressionCoefficients,
      expressionConstant = expression.expressionConstant P.* scale
    }

multiplyExpressions :: AffineExpression -> AffineExpression -> Maybe AffineExpression
multiplyExpressions expression1 expression2 =
  case (constantExpressionValue expression1, constantExpressionValue expression2) of
    (Just constant1, _) -> Just $ scaleExpression constant1 expression2
    (_, Just constant2) -> Just $ scaleExpression constant2 expression1
    _ -> Nothing

constantExpressionValue :: AffineExpression -> Maybe Rational
constantExpressionValue expression
  | Map.null expression.expressionCoefficients = Just expression.expressionConstant
  | otherwise = Nothing

lineariseExactExpr :: ExprStore -> ExprHash -> Maybe AffineExpression
lineariseExactExpr exprNodes root =
  Map.findWithDefault Nothing root
    $ evalExprWith
      lineariseNode
      Expr.Expr {Expr.nodes = exprNodes, Expr.root = root}
      Map.empty
  where
    lineariseNode _ node =
      case node of
        ExprVar {var} -> Just $ linearVar var
        ExprLit {lit} -> Just $ linearConst lit
        ExprUnary {unop = OpNeg, e1} -> negateExpression <$> e1
        ExprUnary {} -> Nothing
        ExprBinary {binop = OpPlus, e1, e2} -> addExpressions <$> e1 <*> e2
        ExprBinary {binop = OpMinus, e1, e2} -> subtractExpressions <$> e1 <*> e2
        ExprBinary {binop = OpTimes, e1, e2} -> do
          expression1 <- e1
          expression2 <- e2
          multiplyExpressions expression1 expression2
        ExprBinary {binop = OpDivide, e1, e2} -> do
          numerator <- e1
          denominator <- e2
          denominatorValue <- constantExpressionValue denominator
          if denominatorValue == 0
            then Nothing
            else Just $ scaleExpression (P.recip denominatorValue) numerator

-- | Sound linear relaxations of a comparison @e1 <= e2@, each meaning
-- @sum coefficients * vars <= rhs@.
type RelaxComparison = ExprHash -> ExprHash -> [LinearRelaxation]

-- | The exact relaxation when both sides of a comparison are linear,
-- otherwise all relaxations given by the nonlinear linearisations.
relaxComparisonWith :: ExprStore -> [RelaxComparison] -> RelaxComparison
relaxComparisonWith exprNodes nonlinearRelaxations e1 e2 =
  case exactRelaxation exprNodes e1 e2 of
    Just relaxation -> [relaxation]
    Nothing -> P.concatMap (\relax -> relax e1 e2) nonlinearRelaxations

exactRelaxation :: ExprStore -> ExprHash -> ExprHash -> Maybe LinearRelaxation
exactRelaxation exprNodes e1 e2 =
  case (lineariseExactExpr exprNodes e1, lineariseExactExpr exprNodes e2) of
    (Just expression1, Just expression2) ->
      let difference = subtractExpressions expression1 expression2
       in Just
            LinearRelaxation
              { coefficients = difference.expressionCoefficients,
                rhs = P.negate difference.expressionConstant
              }
    _ -> Nothing

-- | Linearisation read off affine-arithmetic values of the compared expressions.
affineRelaxations :: ExprStore -> Map.Map ExprHash MPAffine -> RelaxComparison
affineRelaxations exprNodes exprValues =
  let maybeSources = sourceVariables exprNodes exprValues
   in \e1 e2 -> maybeToList $ do
        value1 <- Map.lookup e1 exprValues
        value2 <- Map.lookup e2 exprValues
        sources <- maybeSources
        pure $ affineToRelaxation sources (value1 - value2)

data SourceVariable = SourceVariable
  { sourceVar :: Var,
    sourceCentre :: Rational,
    sourceRadius :: Rational
  }

sourceVariables :: ExprStore -> Map.Map ExprHash MPAffine -> Maybe (Map.Map ErrorTermId SourceVariable)
sourceVariables exprNodes exprValues =
  P.foldl addSource (Just Map.empty) sources
  where
    sources =
      [ (errorId, SourceVariable {sourceVar = var, sourceCentre = rational affine.centre, sourceRadius = rational radius})
        | (exprHash, ExprVar {var}) <- Map.toList exprNodes,
          Just affine <- [Map.lookup exprHash exprValues],
          [(errorId, radius)] <- [Map.toList affine.errTerms],
          radius /= 0
      ]

    addSource Nothing _ = Nothing
    addSource (Just sourcesSoFar) (errorId, source)
      | Map.member errorId sourcesSoFar = Nothing
      | otherwise = Just $ Map.insert errorId source sourcesSoFar

affineToRelaxation :: Map.Map ErrorTermId SourceVariable -> MPAffine -> LinearRelaxation
affineToRelaxation sources affine =
  LinearRelaxation
    { coefficients = Map.fromListWith (P.+) sourceCoefficients,
      rhs = residualRadius P.- constant
    }
  where
    -- Replace each source noise symbol using x = centre + radius * epsilon.
    -- All other symbols stay independent and contribute their absolute radius.
    (sourceTerms, residualTerms) = P.foldr classifyTerm ([], []) (Map.toList affine.errTerms)

    classifyTerm (errorId, coefficient) (sourceAcc, residualAcc) =
      case Map.lookup errorId sources of
        Just source -> ((source, rational coefficient) : sourceAcc, residualAcc)
        Nothing -> (sourceAcc, rational coefficient : residualAcc)

    sourceCoefficients =
      [ (source.sourceVar, coefficient P./ source.sourceRadius)
        | (source, coefficient) <- sourceTerms
      ]
    centreAdjustment =
      sum
        [ (coefficient P./ source.sourceRadius) P.* source.sourceCentre
          | (source, coefficient) <- sourceTerms
        ]
    constant = rational affine.centre P.- centreAdjustment
    residualRadius = sum (P.abs <$> residualTerms)

-- | Linearisation from the box corners using interval automatic differentiation.
-- The known values are enclosures of expressions over the box, used to tighten
-- the values computed during differentiation.
adCornerRelaxations :: Box -> ExprStore -> Map.Map ExprHash MP.MPBall -> RelaxComparison
adCornerRelaxations scope exprNodes exprValues e1 e2 =
  case derivativeValue of
    Just derivative ->
      case (evalAtCorner leftDomains, evalAtCorner rightDomains) of
        (Just valueAtLeft, Just valueAtRight) ->
          [ leftRelaxation valueAtLeft derivative.partials,
            rightRelaxation valueAtRight derivative.partials
          ]
        _ -> []
    Nothing -> []
  where
    domains = scope.box_.varDomains
    leftDomains = Map.map pointAtLower domains
    rightDomains = Map.map pointAtUpper domains

    -- The comparison e1 <= e2 is represented as t = e2 - e1 >= 0.
    derivativeValue = subtractIntervalAD <$> evalIntervalAD exprNodes exprValues domains e2 <*> evalIntervalAD exprNodes exprValues domains e1

    evalAtCorner cornerDomains = do
      value1 <- Map.lookup e1 values
      value2 <- Map.lookup e2 values
      pure $ value2 P.- value1
      where
        cornerBox = boxWithHash $ scope.box_ {varDomains = cornerDomains}
        valuesAfterE1 =
          evalExpr
            sampleBall
            cornerBox
            Expr.Expr {Expr.nodes = exprNodes, Expr.root = e1}
            Map.empty
        values =
          evalExpr
            sampleBall
            cornerBox
            Expr.Expr {Expr.nodes = exprNodes, Expr.root = e2}
            valuesAfterE1

    sampleBall = mpBallP precision (rational 0)
    precision = case Map.elems domains of
      domain : _ -> getPrecision domain
      [] -> MP.defaultPrecision

    domainBounds =
      [ (var, rational lower, rational upper)
        | (var, domain) <- Map.toList domains,
          let (lower, upper) = MP.endpoints domain
      ]

    derivativeBounds partialMap var =
      case Map.lookup var partialMap of
        Just derivative -> mpBallBounds derivative
        Nothing -> (rational 0, rational 0)

    upperBound = P.snd . mpBallBounds

    leftRelaxation valueAtLeft partialMap =
      LinearRelaxation
        { coefficients = Map.fromList [(var, P.negate derivativeUpper) | (var, _, _) <- domainBounds, let (_, derivativeUpper) = derivativeBounds partialMap var],
          rhs = upperBound valueAtLeft P.- sum [derivativeUpper P.* lower | (var, lower, _) <- domainBounds, let (_, derivativeUpper) = derivativeBounds partialMap var]
        }

    rightRelaxation valueAtRight partialMap =
      LinearRelaxation
        { coefficients = Map.fromList [(var, P.negate derivativeLower) | (var, _, _) <- domainBounds, let (derivativeLower, _) = derivativeBounds partialMap var],
          rhs = upperBound valueAtRight P.- sum [derivativeLower P.* upper | (var, _, upper) <- domainBounds, let (derivativeLower, _) = derivativeBounds partialMap var]
        }

    pointAtLower = MP.endpointLAsInterval
    pointAtUpper = MP.endpointRAsInterval

