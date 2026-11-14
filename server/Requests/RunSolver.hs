{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Requests.RunSolver
  ( RunSolverRequest (..),
    Arithmetic (..),
    SolverRunStatus (..),
    SolverRunStatusUpdate (..),
  )
where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (..), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Exception (evaluate, finally)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPBPParams (..), LPPPruningMethod (..), LPPStep, getStepBoxes, lppBranchAndPrune)
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), exampleProblems, substituteParams)
import LPPaver2.Export ()
import LPPaver2.RealConstraints (EvalArithmetic (..), EvaluatedForm)
import LPPaver2.RealConstraints.Boxes (BoxStore)
import MixedTypesNumPrelude (convert, convertExactly)
import RequestHandler (HandlerContinuation (..), IsRequestResponse (..), ModifyState, RequestHandlerInfo (..), aesonOptions)
import ServerState (LPPStepNoEvalInfo, RunID (..), RunTiming (..))
import ServerState qualified
import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)
import Prelude

data Arithmetic
  = BallArithmetic {precision :: Integer}
  | AffineArithmetic {precision :: Integer, maxTerms :: Int}
  deriving (Generic, Show)

getEvalArithmetic :: Arithmetic -> EvalArithmetic
getEvalArithmetic (BallArithmetic {precision}) =
  EvalArithmeticMPBall {sampleBall = MP.mpBallP (MP.prec precision) (0 :: Integer)}
getEvalArithmetic (AffineArithmetic {precision, maxTerms}) =
  EvalArithmeticAffine
    { sampleAffine =
        MPAffine
          { config = MPAffineConfig {maxTerms = maxTerms, precision = precision},
            centre = convertExactly (0 :: Integer),
            errTerms = Map.empty
          }
    }

data RunSolverRequest = RunSolverRequest
  { runId :: RunID,
    problemName :: String,
    paramValues :: Map.Map String Double,
    arithmetic :: Arithmetic,
    useLinearPrune :: Bool,
    useSimplexAA :: Bool,
    useSimplexAD :: Bool,
    giveUpAccuracy :: Double,
    numberOfThreads :: Int
  }
  deriving (Generic, Show)

data SolverRunStatus = SolverRunning | SolverFinished
  deriving (Generic, Show)

data SolverRunStatusUpdate = SolverRunStatusUpdate
  { runId :: RunID,
    status :: SolverRunStatus,
    -- | How long the run has taken so far, or how long it took in total if the status is SolverFinished.
    timing :: RunTiming,
    -- | The steps come without evaluation info, clients get it on demand using GetEvalInfoRequest.
    newSteps :: [LPPStepNoEvalInfo],
    newBoxes :: BoxStore
  }
  deriving (Generic)

instance IsRequestResponse RunSolverRequest where
  type ResponseType RunSolverRequest = SolverRunStatusUpdate
  handleRequest RequestHandlerInfo {request, modifyState, respond, addStateChangeHandler, isConnectionClosed} = do
    stateAtStart <- modifyState $ \state -> let newState = ServerState.startRun runId state in (newState, newState)
    -- start measuring the run's wall clock and CPU time
    getTimingSoFar <- startTiming
    -- report solver has started
    timingAtStart <- getTimingSoFar
    respond $ SolverRunStatusUpdate {runId, status = SolverRunning, timing = timingAtStart, newSteps = [], newBoxes = Map.empty}
    -- the publisher thread will report progress, coalescing steps reported in quick succession
    addStateChangeHandler stateAtStart (progressHandler getTimingSoFar)

    -- run the solver with our steps controller
    -- whatever happens, mark the run as finished so that the publisher reports any remaining steps and then that the solver has finished
    -- the run's duration is taken here, when the solver stops, not when the publisher gets round to reporting it
    let finishRun = do
          finalTiming <- getTimingSoFar
          modifyState $ \state -> (ServerState.finishRun runId finalTiming state, ())
    flip finally finishRun $ do
      result <- runStdoutLoggingT $ do
        lppBranchAndPrune
          ( LPPPruningMethod
              { evalArithmetic = getEvalArithmetic request.arithmetic,
                useLinearPrune = request.useLinearPrune,
                useSimplexAA = request.useSimplexAA,
                useSimplexAD = request.useSimplexAD,
                giveUpAccuracy = convert request.giveUpAccuracy
              }
          )
          (lppStepsController runId modifyState) -- accummulates steps and boxes in the server state
          (mkParams request) {shouldAbort = abortWhen isConnectionClosed "client disconnected"}
      forM_ result.aborted $ \reason ->
        putStrLn $ "Solver run " ++ show runId ++ " aborted: " ++ reason
    where
      runId = request.runId
      progressHandler getTimingSoFar oldState newState = do
        let newSteps = ServerState.getNewRunSteps runId oldState newState
            newBoxes = Map.unions (map getStepBoxes newSteps)
        timingSoFar <- getTimingSoFar
        unless (null newSteps) $
          respond $
            SolverRunStatusUpdate {runId, status = SolverRunning, timing = timingSoFar, newSteps, newBoxes}
        case ServerState.isRunFinished runId newState of
          True -> do
            let finalTiming = fromMaybe timingSoFar (ServerState.getRunFinalTiming runId newState)
            respond $ SolverRunStatusUpdate {runId, status = SolverFinished, timing = finalTiming, newSteps = [], newBoxes = Map.empty}
            pure RemoveHandler
          False -> pure KeepHandler

-- | Start a stopwatch, returning an action that reads the wall clock and CPU time elapsed since.
-- The CPU time is that of the whole process, ie summed over all threads.
startTiming :: IO (IO RunTiming)
startTiming = do
  wallStart <- getMonotonicTime
  cpuStart <- getCPUTime
  pure $ do
    wallNow <- getMonotonicTime
    cpuNow <- getCPUTime
    pure $
      RunTiming
        { wallSeconds = wallNow - wallStart,
          cpuSeconds = fromIntegral (cpuNow - cpuStart) / 1e12 -- getCPUTime is in picoseconds
        }

lppStepsController :: (MonadIO m) => RunID -> ModifyState () -> BP.StepsController m (LPPStep EvaluatedForm)
lppStepsController runId modifyState =
  BP.StepsController {reportStep}
  where
    -- only record the step; sending it to the client is done by the publisher thread
    reportStep step = liftIO $ do
      -- Force the step's boxes here, in the solver thread, before taking the state lock.
      -- Pruning and splitting are lazy and would otherwise be evaluated inside modifyState,
      -- ie while holding the lock, which would serialise the solver threads.
      stepBoxes <- evaluate (getStepBoxes step)
      modifyState $ \state -> (ServerState.addNewSteps runId [step] stepBoxes state, ())

-- | Turn an IO condition into a B&P shouldAbort function.
-- The B&P engine requires a pure function, so we read the condition with unsafePerformIO.
-- This is safe because reading a flag has no side effects and a stale value only delays the abort by a step.
-- NOINLINE and the dependency on the paving argument ensure the condition is re-read on every call
-- instead of being floated out and evaluated only once.
{-# NOINLINE abortWhen #-}
abortWhen :: IO Bool -> String -> paving -> Maybe String
abortWhen condition reason paving =
  unsafePerformIO $ do
    shouldAbort <- paving `seq` condition
    pure $ case shouldAbort of
      True -> Just reason
      False -> Nothing

mkParams :: RunSolverRequest -> LPPBPParams
mkParams request =
  LPPBPParams
    { problem = problemWithSubstitutedParams,
      maxThreads = request.numberOfThreads,
      giveUpAccuracy = convert request.giveUpAccuracy,
      shouldAbort = const Nothing,
      shouldLog = False
    }
  where
    problemWithSubstitutedParams = case Map.lookup request.problemName exampleProblems of
      Just (LPPProblemWithParamSpec {problem}) ->
        let paramValues = Map.map convert request.paramValues
            substitutedProblem = substituteParams problem paramValues
         in substitutedProblem
      Nothing -> error $ "Problem not found: " ++ request.problemName

instance A.FromJSON RunSolverRequest where
  parseJSON = A.genericParseJSON aesonOptions

instance A.FromJSON Arithmetic where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON SolverRunStatus where
  toEncoding = A.genericToEncoding aesonOptions

instance A.ToJSON SolverRunStatusUpdate where
  toEncoding = A.genericToEncoding aesonOptions
