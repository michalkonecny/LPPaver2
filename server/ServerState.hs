module ServerState
  ( RunID (..),
    RunInfo (..),
    RunTiming (..),
    LPPStepNoEvalInfo,
    ProblemKey,
    ServerState (..),
    new,
    addBoxes,
    addForms,
    addNewSteps,
    startRun,
    finishRun,
    getNewRunSteps,
    lookupEvalInfo,
    isRunFinished,
    getRunFinalTiming,
  )
where

import Data.Aeson qualified as A
import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import GHC.Records
import BranchAndPrune.BranchAndPrune qualified as BP
import LPPaver2.BranchAndPrune (LPPStep)
import LPPaver2.RealConstraints (Box (..), BoxHash, BoxStore, EvaluatedForm, ExprStore, Form (..), FormHash, FormStore)
import Prelude

-- All fields are strict so that forcing a state to WHNF also forces the spines of its maps,
-- preventing thunk build-up inside the state MVar.
data ServerState = ServerState
  { boxes :: !BoxStore,
    exprs :: !ExprStore,
    forms :: !FormStore,
    runs :: !(Map.Map RunID RunInfo)
  }

newtype RunID = RunID String
  deriving (Eq, Ord, Show, Generic)

instance A.FromJSON RunID where
  parseJSON = A.genericParseJSON A.defaultOptions

instance A.ToJSON RunID where
  toEncoding = A.genericToEncoding A.defaultOptions

-- | A step without its evaluation info.
-- This is what clients are sent as the solver progresses; the evaluation info is sent only on demand.
type LPPStepNoEvalInfo = LPPStep ()

-- | Identifies a problem within a run by the hashes of its scope and constraint.
type ProblemKey = (BoxHash, FormHash)

data RunInfo = RunInfo
  { runID :: !RunID,
    steps :: !(Seq LPPStepNoEvalInfo), -- all steps reported so far, in order
    -- | The evaluation info of each progress step, indexed by the step's problem.
    evalInfos :: !(Map.Map ProblemKey EvaluatedForm),
    finished :: !Bool,
    -- | How long the run took, set when the run finishes.
    finalTiming :: !(Maybe RunTiming)
  }

-- | How long a solver run has taken.
data RunTiming = RunTiming
  { -- | Elapsed wall clock time in seconds.
    wallSeconds :: !Double,
    -- | CPU time in seconds used by the whole server process, summed over all its threads.
    -- Besides the solver threads, this includes garbage collection and anything else the server did meanwhile.
    cpuSeconds :: !Double
  }
  deriving (Generic)

instance A.ToJSON RunTiming where
  toEncoding = A.genericToEncoding A.defaultOptions

new :: ServerState
new =
  ServerState
    { boxes = Map.empty,
      exprs = Map.empty,
      forms = Map.empty,
      runs = Map.empty
    }

addBoxes :: [Box] -> ServerState -> ServerState
addBoxes newBoxes state =
  state {boxes = state.boxes `Map.union` newBoxesMap}
  where
    newBoxesMap = Map.fromList [(b.boxHash, b) | b <- newBoxes]

addForms :: [Form] -> ServerState -> ServerState
addForms newForms state =
  state
    { exprs = Map.unions $ state.exprs : newExprNodes,
      forms = Map.unions $ state.forms : newFormNodes
    }
  where
    newExprNodes = List.map (\f -> f.nodesE) newForms
    newFormNodes = List.map (\f -> f.nodesF) newForms

-- | Record new steps for a run, together with their boxes and formula nodes.
-- This is called by the solver threads for every step, so it needs to be cheap.
addNewSteps :: RunID -> [LPPStep EvaluatedForm] -> BoxStore -> ServerState -> ServerState
addNewSteps runId newSteps newBoxes state =
  addForms newForms $
    addBoxes (Map.elems newBoxes) $
      state {runs = Map.alter (Just . addSteps) runId state.runs}
  where
    newForms = List.map (.constraint) $ List.concatMap BP.getStepProblems newSteps
    addSteps maybeRunInfo =
      runInfo
        { steps = runInfo.steps <> Seq.fromList (List.map dropEvalInfo newSteps),
          evalInfos = Map.fromList (List.concatMap stepEvalInfo newSteps) `Map.union` runInfo.evalInfos
        }
      where
        runInfo = maybe (emptyRunInfo runId) id maybeRunInfo

emptyRunInfo :: RunID -> RunInfo
emptyRunInfo runId =
  RunInfo {runID = runId, steps = Seq.empty, evalInfos = Map.empty, finished = False, finalTiming = Nothing}

dropEvalInfo :: LPPStep EvaluatedForm -> LPPStepNoEvalInfo
dropEvalInfo step = case step of
  BP.InitStep {problem} -> BP.InitStep {problem}
  BP.ProgressStep {problem, progressPaving} -> BP.ProgressStep {problem, progressPaving, evalInfo = ()}
  BP.GiveUpOnProblemStep {problem} -> BP.GiveUpOnProblemStep {problem}
  BP.AbortStep {detail} -> BP.AbortStep {detail}
  BP.DoneStep -> BP.DoneStep

stepEvalInfo :: LPPStep EvaluatedForm -> [(ProblemKey, EvaluatedForm)]
stepEvalInfo step = case step of
  BP.ProgressStep {problem, evalInfo} -> [((problem.scope.boxHash, problem.constraint.root), evalInfo)]
  _ -> []

startRun :: RunID -> ServerState -> ServerState
startRun runId state =
  state {runs = Map.insert runId (emptyRunInfo runId) state.runs}

finishRun :: RunID -> RunTiming -> ServerState -> ServerState
finishRun runId timing state =
  state {runs = Map.adjust (\runInfo -> runInfo {finished = True, finalTiming = Just timing}) runId state.runs}

-- | The steps of the run that are in the new state but not in the old state.
-- Cheap, because steps are only ever appended.
getNewRunSteps :: RunID -> ServerState -> ServerState -> [LPPStepNoEvalInfo]
getNewRunSteps runId oldState newState =
  case Map.lookup runId newState.runs of
    Nothing -> []
    Just newRunInfo -> toList $ Seq.drop oldCount newRunInfo.steps
  where
    oldCount = maybe 0 (Seq.length . (.steps)) (Map.lookup runId oldState.runs)

-- | The evaluation info of the progress step that solved the given problem in the given run, if any.
lookupEvalInfo :: RunID -> ProblemKey -> ServerState -> Maybe EvaluatedForm
lookupEvalInfo runId problemKey state =
  Map.lookup runId state.runs >>= \runInfo -> Map.lookup problemKey runInfo.evalInfos

isRunFinished :: RunID -> ServerState -> Bool
isRunFinished runId state = maybe False (.finished) (Map.lookup runId state.runs)

-- | How long the run took, available once the run has finished.
getRunFinalTiming :: RunID -> ServerState -> Maybe RunTiming
getRunFinalTiming runId state = Map.lookup runId state.runs >>= (.finalTiming)
