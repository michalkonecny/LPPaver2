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
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ForkUtils (MonadUnliftIOWithState)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints
import MixedTypesNumPrelude
import Text.Printf (printf)

type LPPProblem = BP.Problem Form Box

type LPPPaving = BP.Paving Form Box Boxes

type LPPStep = BP.Step LPPProblem LPPPaving

getStepBoxes :: LPPStep -> BoxStore
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

type LPPBPResult = BP.Result Form Box Boxes

data LPPBPParams = LPPBPParams
  { problem :: LPPProblem,
    maxThreads :: Int,
    giveUpAccuracy :: Rational,
    shouldLog :: Bool
  }

shouldGiveUpOnBPLPPProblem :: Rational -> LPPProblem -> Bool
shouldGiveUpOnBPLPPProblem giveUpAccuracy (BP.Problem {scope}) =
  all smallerThanPrec (Map.elems scope.box_.varDomains)
  where
    smallerThanPrec :: MPBall -> Bool
    smallerThanPrec ball = diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

lppBranchAndPrune ::
  ( MonadLogger m,
    MonadIO m,
    MonadUnliftIOWithState m,
    CanEval r,
    HasKleenanComparison r,
    BP.CanControlSteps m LPPStep
  ) =>
  r ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPrune sampleR (LPPBPParams {..}) = do
  -- conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  BP.branchAndPruneM
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = sampleR,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBPLPPProblem giveUpAccuracy :: LPPProblem -> Bool,
          BP.dummyPriorityQueue,
          BP.maxThreads,
          BP.shouldLog
        }
    )
  where
    dummyPriorityQueue :: BoxStack
    dummyPriorityQueue = BoxStack [problem]

instance
  (CanEval r, HasKleenanComparison r, Applicative m) =>
  BP.CanPrune m r Form Box Boxes
  where
  pruneProblemM sampleR (BP.Problem {scope, constraint}) = pure pavingP
    where
      result = simplifyEvalForm sampleR scope constraint
      simplifiedConstraint = result.evaluatedForm.form
      mkBoxes box = Boxes {store = Map.fromList [(box.boxHash, box)]}
      pavingP :: BP.Paving Form Box Boxes
      pavingP = case getFormDecision simplifiedConstraint of
        CertainTrue -> BP.pavingInner scope (mkBoxes scope)
        CertainFalse -> BP.pavingOuter scope (mkBoxes scope)
        _ -> BP.pavingUndecided scope [BP.Problem {scope, constraint = simplifiedConstraint}]

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

instance BP.CanSplitProblem constraint Box where
  splitProblem (BP.Problem {scope, constraint}) =
    map (\box -> BP.Problem {scope = box, constraint}) $ splitBox scope
