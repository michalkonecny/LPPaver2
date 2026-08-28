{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use >" #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (..), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune (Problem (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPBPParams (..), LPPStep, getStepBoxes, getStepExprs, getStepForms, lppBranchAndPrune)
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), exampleProblems, exampleProblemsList, substituteParams)
import LPPaver2.Export ()
import LPPaver2.RealConstraints (EvalArithmetic (..), ExprStore, FormStore)
import LPPaver2.RealConstraints.Boxes (BoxStore)
import MixedTypesNumPrelude (convert, convertExactly)
import Network.WebSockets qualified as WS
import ServerState (RunID (..), RunInfo (..), ServerState (..))
import ServerState qualified
import Prelude

main :: IO ()
main = do
  putStrLn "Starting LPPaver2 server."
  state <- newMVar ServerState.new
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application stateMVar pending = do
  conn <- WS.acceptRequest pending
  putStrLn "Client connected."
  -- withPingThread conn 30 (return ()) (forever (requestResponse conn))
  forever $ requestResponse stateMVar conn

requestResponse :: MVar ServerState -> WS.Connection -> IO ()
requestResponse stateMVar conn = do
  putStrLn "waiting for message from client..."
  msg <- WS.receiveData conn :: IO Text
  -- putStrLn $ "Received message: " ++ T.unpack msg
  -- TODO: fork ?
  request <- parseRequest msg
  modifyMVar stateMVar $ \state -> do
    newState <- handleRequest state request respond
    return (newState, ())
  where
    respond :: Response -> IO ()
    respond response = do
      let responseJSON = A.encode response
      -- putStrLn $ "Sending response: " ++ TL.unpack (TL.decodeUtf8 responseJSON)
      WS.sendTextData conn responseJSON

------------------------
--- Example problems ---
------------------------

data GetExampleProblemsRequest = GetExampleProblemsRequest
  deriving (Generic, Show)

data ExampleProblemsResponse = ExampleProblemsResponse
  { problems :: [(String, LPPProblemWithParamSpec)],
    boxes :: BoxStore
  }
  deriving (Generic)

instance IsRequestResponse GetExampleProblemsRequest where
  type ResponseType GetExampleProblemsRequest = ExampleProblemsResponse
  handleRequest state _ respond = do
    let problems = exampleProblemsList
    let scopes = map (\(_, p) -> p.problem.scope) problems
    let problemForms = map (\(_, p) -> p.problem.constraint) problems
    let newState = ServerState.addBoxes scopes $ ServerState.addForms problemForms state
    respond $ ExampleProblemsResponse {problems = problems, boxes = newState.boxes}
    pure newState

instance A.FromJSON GetExampleProblemsRequest where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON ExampleProblemsResponse where
  toEncoding = A.genericToEncoding aesonOptions

--------------------------------
--- Formula/expression nodes ---
--------------------------------

data GetAllFormulaNodesRequest = GetAllFormulaNodesRequest
  deriving (Generic, Show)

data FormulaNodesResponse = FormulaNodesResponse
  { exprs :: ExprStore,
    forms :: FormStore
  }
  deriving (Generic)

instance IsRequestResponse GetAllFormulaNodesRequest where
  type ResponseType GetAllFormulaNodesRequest = FormulaNodesResponse
  handleRequest state _ respond = do
    respond $ FormulaNodesResponse {exprs = state.exprs, forms = state.forms}
    pure state

instance A.FromJSON GetAllFormulaNodesRequest where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON FormulaNodesResponse where
  toEncoding = A.genericToEncoding aesonOptions

------------------------------------------------
--- Running the solver and returning results ---
------------------------------------------------

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
    giveUpAccuracy :: Double,
    numberOfThreads :: Int
  }
  deriving (Generic, Show)

data SolverRunStatus = SolverRunning | SolverFinished
  deriving (Generic, Show)

data SolverRunStatusUpdate = SolverRunStatusUpdate
  { runId :: RunID,
    status :: SolverRunStatus,
    newSteps :: [LPPStep],
    newBoxes :: BoxStore
  }
  deriving (Generic)

instance IsRequestResponse RunSolverRequest where
  type ResponseType RunSolverRequest = SolverRunStatusUpdate
  handleRequest state request respond = do
    let runId = request.runId
    respond (SolverRunStatusUpdate {runId, status = SolverRunning, newSteps = [], newBoxes = Map.empty})
    let params = mkParams request
    stateMV <- liftIO $ newMVar state -- all updates are done via modifyMVar to ensure atomicity
    -- run the solver and add steps and boxes into stateMV as they are generated
    _ <- runStdoutLoggingT $ do
      lppBranchAndPrune (getEvalArithmetic request.arithmetic) (lppStepsController runId stateMV) params
    -- after the solver finishes, extract the new steps and boxes ...
    (newSteps, newBoxes) <- modifyMVar stateMV $ \state2 -> do
      pure $ ServerState.processNewSteps runId state2
    -- ... and send them to the client as part of the final status update
    respond (SolverRunStatusUpdate {runId, status = SolverFinished, newSteps, newBoxes})
    liftIO $ takeMVar stateMV

-- TODO: send the steps at around 2 Hz in chunks as they are generated,
-- instead of waiting for the solver to finish

lppStepsController :: (MonadIO m) => RunID -> MVar ServerState -> BP.StepsController m LPPStep
lppStepsController runId stateMV =
  BP.StepsController {reportStep}
  where
    reportStep step = liftIO $ do
      modifyMVar_ stateMV (pure . addStepToState step)
      putStrLn $ "Step for runId " ++ show runId ++ ": " ++ show step

    addStepToState :: LPPStep -> ServerState -> ServerState
    addStepToState step state =
      state
        { -- Update the server state with the new boxes, expressions, and forms from the step
          boxes = Map.union state.boxes stepBoxes,
          exprs = Map.union state.exprs (getStepExprs step),
          forms = Map.union state.forms (getStepForms step),
          runs = Map.alter updateRunInfo runId state.runs
        }
      where
        stepBoxes = getStepBoxes step
        updateRunInfo :: Maybe RunInfo -> Maybe RunInfo
        updateRunInfo Nothing =
          Just $
            RunInfo
              { runID = runId,
                steps = [],
                newSteps = [step],
                newBoxes = stepBoxes
              }
        updateRunInfo (Just runInfo) =
          Just $
            RunInfo
              { runID = runInfo.runID,
                steps = runInfo.steps,
                newSteps = runInfo.newSteps ++ [step],
                newBoxes = Map.union runInfo.newBoxes stepBoxes
              }

mkParams :: RunSolverRequest -> LPPBPParams
mkParams request =
  LPPBPParams
    { problem = problemWithSubstitutedParams,
      maxThreads = request.numberOfThreads,
      giveUpAccuracy = convert request.giveUpAccuracy,
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

------------------------------------------------
--- Request/Response boilerplate and parsing ---
------------------------------------------------

class IsRequestResponse request where
  type ResponseType request
  handleRequest ::
    ServerState ->
    request ->
    (ResponseType request -> IO ()) ->
    IO ServerState

data Request
  = RequestGetExampleProblems GetExampleProblemsRequest
  | RequestGetAllFormulaNodes GetAllFormulaNodesRequest
  | RequestRunSolver RunSolverRequest
  deriving (Generic, Show)

data Response
  = ResponseExampleProblems ExampleProblemsResponse
  | ResponseFormulaNodes FormulaNodesResponse
  | ResponseSolverRunStatusUpdate SolverRunStatusUpdate
  deriving (Generic)

instance IsRequestResponse Request where
  type ResponseType Request = Response
  handleRequest state (RequestGetExampleProblems req) respond = do
    handleRequest state req (respond . ResponseExampleProblems)
  handleRequest state (RequestGetAllFormulaNodes req) respond = do
    handleRequest state req (respond . ResponseFormulaNodes)
  handleRequest state (RequestRunSolver req) respond = do
    handleRequest state req (respond . ResponseSolverRunStatusUpdate)

instance A.FromJSON Request where
  parseJSON = A.genericParseJSON aesonOptions

parseRequest :: Text -> IO Request
parseRequest msg =
  let msgBS = T.encodeUtf8 msg
   in case A.eitherDecodeStrict msgBS of
        Right req -> do
          putStrLn $ "Parsed: " ++ show req
          return req
        Left err -> do
          putStrLn $ "Failed to parse request: " ++ err
          fail $ "Failed to parse request: " ++ err

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding aesonOptions

aesonOptions :: A.Options
aesonOptions = A.defaultOptions
