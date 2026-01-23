{-# LANGUAGE UndecidableInstances #-}

module LPPaver2.BranchAndPrune
  ( --
    LPPProblem,
    LPPPaving,
    LPPStep,
    LPPBPResult,
    LPPBPParams (..),
    lppBranchAndPrune,
    getStepBoxes,
    getStepExprs,
    getStepForms,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ForkUtils (MonadUnliftIOWithState)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Hashable (Hashable (hash))
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints
import MixedTypesNumPrelude
import Text.Printf (printf)

type LPPProblem = BP.Problem Form Box

type LPPPaving = BP.Paving Form Box Boxes

type LPPStep r = BP.Step LPPProblem LPPPaving (EvaluatedForm r)

getStepBoxes :: LPPStep r -> BoxStore
getStepBoxes step =
  scopesStore `Map.union` pavingBoxStore
  where
    scopesStore = boxListToStore $ problemsScopes <> pavingsScopes
    boxListToStore :: [Box] -> BoxStore
    boxListToStore boxes = Map.fromList [(box.boxHash, box) | box <- boxes]
    problems = BP.getStepProblems step
    problemsScopes = [p.scope | p <- problems]
    pavings = BP.getStepPavings step
    pavingsScopes = [p.scope | p <- pavings]
    pavingBoxStore = Map.unions [paving.inner.store `Map.union` paving.outer.store | paving <- pavings]

getStepExprs :: LPPStep r -> ExprStore
getStepExprs step =
  constraintsStore `Map.union` undecidedStore
  where
    constraintsStore = Map.unions [prob.constraint.nodesE | prob <- problems]
    undecidedStore =
      Map.unions
        [ prob.constraint.nodesE
          | paving <- pavings,
            prob <- paving.undecided
        ]
    problems = BP.getStepProblems step
    pavings = BP.getStepPavings step

getStepForms :: LPPStep r -> FormStore
getStepForms step =
  constraintsStore `Map.union` undecidedStore `Map.union` basicFormStore
  where
    constraintsStore = Map.unions [prob.constraint.nodesF | prob <- problems]
    undecidedStore =
      Map.unions
        [ prob.constraint.nodesF
          | paving <- pavings,
            prob <- paving.undecided
        ]
    problems = BP.getStepProblems step
    pavings = BP.getStepPavings step

basicFormStore :: FormStore
basicFormStore =
  Map.fromList
    [ (FormHash (hash (FormTrue :: FormF FormHash)), FormTrue),
      (FormHash (hash (FormFalse :: FormF FormHash)), FormFalse)
    ]

type LPPBPResult = BP.Result Form Box Boxes

data LPPBPParams = LPPBPParams
  { problem :: LPPProblem,
    maxThreads :: Int,
    giveUpAccuracy :: Rational,
    shouldLog :: Bool
  }

shouldGiveUpOnBPLPPProblem :: Rational -> LPPProblem -> Bool
shouldGiveUpOnBPLPPProblem giveUpAccuracy (BP.Problem {scope}) =
  all smallerThanPrec domainsOfSplitVars
  where
    domainsOfSplitVars =
      [ ball
        | var <- scope.box_.splitOrder,
          Just ball <- [Map.lookup var scope.box_.varDomains]
      ]

    smallerThanPrec :: MPBall -> Bool
    smallerThanPrec ball = diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

lppBranchAndPrune ::
  ( MonadLogger m,
    MonadIO m,
    MonadUnliftIOWithState m,
    CanEval r,
    HasKleeneanComparison r,
    BP.CanControlSteps m (LPPStep r)
  ) =>
  r ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPrune (sampleR :: r) (LPPBPParams {..}) = do
  -- conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  BP.branchAndPruneM
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = sampleR,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBPLPPProblem giveUpAccuracy :: LPPProblem -> Bool,
          BP.dummyPriorityQueue,
          BP.dummyEvalInfo = EvaluatedForm {form = formTrue, exprValues = Map.empty, formValues = Map.empty} :: EvaluatedForm r,
          BP.maxThreads,
          BP.shouldLog
        }
    )
  where
    dummyPriorityQueue :: BoxStack
    dummyPriorityQueue = BoxStack [problem]

instance
  (CanEval r, HasKleeneanComparison r, Applicative m) =>
  BP.CanPrune m r Form Box Boxes (EvaluatedForm r)
  where
  pruneProblemM sampleR (BP.Problem {scope, constraint}) = pure (pavingP, result.evaluatedForm)
    where
      result = simplifyEvalForm sampleR scope constraint
      simplifiedForm = result.evaluatedForm.form
      simplifiedScope = boxRestrictSplitOrder (formVariables simplifiedForm) scope
      mkBoxes box = Boxes {store = Map.fromList [(box.boxHash, box)]}
      pavingP :: BP.Paving Form Box Boxes
      pavingP = case getFormDecision simplifiedForm of
        CertainTrue -> BP.pavingInner scope (mkBoxes scope)
        CertainFalse -> BP.pavingOuter scope (mkBoxes scope)
        _ -> BP.pavingUndecided scope [BP.Problem {scope = simplifiedScope, constraint = simplifiedForm}]

newtype BoxStack = BoxStack [LPPProblem]

instance BP.IsPriorityQueue BoxStack LPPProblem where
  singletonQueue e = BoxStack [e]
  queueToList (BoxStack list) = list
  queuePickNext (BoxStack []) = Nothing
  queuePickNext (BoxStack (e : es)) = Just (e, BoxStack es)
  queueAddMany (BoxStack es) new_es = BoxStack (new_es ++ es)
  queueSplit (BoxStack es)
    | splitPoint == 0 = Nothing
    | otherwise = Just (BoxStack esL, BoxStack esR)
    where
      splitPoint = length es `divI` 2
      (esL, esR) = splitAt splitPoint es

  queueMerge (BoxStack stackL) (BoxStack stackR) = BoxStack $ stackL ++ stackR

instance BP.ShowStats (BP.Subset Boxes Box) where
  showStats (BP.Subset {..}) =
    printf "{|boxes| = %d, coverage = %3.4f%%}" (boxesCount subset) coveragePercent
    where
      coveragePercent = 100 * (boxesAreaD subset / boxAreaD superset)

instance BP.IsSet Boxes where
  emptySet = Boxes {store = Map.empty}
  setIsEmpty (Boxes {store}) = Map.null store
  setUnion bs1 bs2 = Boxes {store = Map.union bs1.store bs2.store}

instance BP.BasicSetsToSet Box Boxes where
  basicSetsToSet list = Boxes {store}
    where
      store = Map.fromList [(box.boxHash, box) | box <- list]

instance BP.CanSplitProblem Form Box where
  splitProblem :: BP.Problem Form Box -> [BP.Problem Form Box]
  splitProblem (BP.Problem {scope, constraint}) =
    map (\box -> BP.Problem {scope = box, constraint}) $ splitBox scope
