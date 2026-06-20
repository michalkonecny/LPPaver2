{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >" #-}

module Main (main) where

import AERN2.MP (MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (..), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune (Problem (..))
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Monad (forever)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPBPParams (..), lppBranchAndPrune)
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), exampleProblems, substituteParams)
import LPPaver2.Export ()
import LPPaver2.RealConstraints (ExprStore, FormStore)
import LPPaver2.RealConstraints.Boxes (BoxStore)
import MixedTypesNumPrelude (convert, convertExactly)
import Network.WebSockets qualified as WS
import ServerState (RunID (..), ServerState (..))
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
  putStrLn $ "Received message: " ++ T.unpack msg
  -- TODO: fork ?
  request <- parseRequest msg
  modifyMVar stateMVar $ \state -> do
    newState <- handleRequest state request respond
    return (newState, ())
  where
    respond :: Response -> IO ()
    respond response = do
      let responseJSON = A.encode response
      putStrLn $ "Sending response: " ++ TL.unpack (TL.decodeUtf8 responseJSON)
      WS.sendTextData conn responseJSON

------------------------
--- Example problems ---
------------------------

data GetExampleProblemsRequest = GetExampleProblemsRequest
  deriving (Generic, Show)

data ExampleProblemsResponse = ExampleProblemsResponse
  { problems :: Map.Map String LPPProblemWithParamSpec,
    boxes :: BoxStore
  }
  deriving (Generic)

instance IsRequestResponse GetExampleProblemsRequest where
  type ResponseType GetExampleProblemsRequest = ExampleProblemsResponse
  handleRequest state _ respond = do
    let problems = exampleProblems
    let scopes = map (\p -> p.problem.scope) $ Map.elems problems
    let problemForms = map (\p -> p.problem.constraint) $ Map.elems problems
    let newState = ServerState.addBoxes scopes $ ServerState.addForms problemForms state
    respond $ ExampleProblemsResponse {problems = problems, boxes = newState.boxes}
    pure newState

instance A.FromJSON GetExampleProblemsRequest where
  parseJSON = A.genericParseJSON A.defaultOptions {A.tagSingleConstructors = True}

instance A.ToJSON ExampleProblemsResponse where
  toEncoding = A.genericToEncoding A.defaultOptions

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
  parseJSON = A.genericParseJSON A.defaultOptions {A.tagSingleConstructors = True}

instance A.ToJSON FormulaNodesResponse where
  toEncoding = A.genericToEncoding A.defaultOptions

------------------------------------------------
--- Running the solver and returning results ---
------------------------------------------------

data Arithmetic = BallArithmetic | AffineArithmetic
  deriving (Generic, Show)

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
    status :: SolverRunStatus
  }
  deriving (Show, Generic)

instance IsRequestResponse RunSolverRequest where
  type ResponseType RunSolverRequest = SolverRunStatusUpdate
  handleRequest state request respond = do
    putStrLn $ "Received RunSolverRequest: " ++ show request
    respond (SolverRunStatusUpdate {runId = request.runId, status = SolverRunning})
    let params = mkParams request
    _ <- runStdoutLoggingT $ case request.arithmetic of
      BallArithmetic -> do
        lppBranchAndPrune sampleMPBall params
      AffineArithmetic -> do
        lppBranchAndPrune sampleMPAffine params
    -- TODO
    respond (SolverRunStatusUpdate {runId = request.runId, status = SolverFinished})
    pure state

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

sampleMPBall :: MPBall
sampleMPBall = MP.mpBallP (MP.prec 1000) (0 :: Integer)

sampleMPAffine :: MPAffine
sampleMPAffine = MPAffine _conf (convertExactly (0 :: Integer)) Map.empty
  where
    _conf :: MPAffineConfig
    _conf = MPAffineConfig {maxTerms = 10, precision = 1000}

instance A.FromJSON RunSolverRequest where
  parseJSON = A.genericParseJSON A.defaultOptions

instance A.FromJSON Arithmetic where
  parseJSON = A.genericParseJSON A.defaultOptions

instance A.ToJSON SolverRunStatus where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON SolverRunStatusUpdate where
  toEncoding = A.genericToEncoding A.defaultOptions

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
  deriving (Generic)

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

parseRequest :: Text -> IO Request
parseRequest msg =
  let msgBS = T.encodeUtf8 msg
   in case A.eitherDecodeStrict msgBS of
        Right req -> do
          putStrLn $ "Parsed GetExampleProblemsRequest: " ++ show req
          return (RequestGetExampleProblems req)
        Left err1 -> do
          case A.eitherDecodeStrict msgBS of
            Right req -> do
              putStrLn $ "Parsed GetAllFormulaNodesRequest: " ++ show req
              return (RequestGetAllFormulaNodes req)
            Left err2 -> do
              case A.eitherDecodeStrict msgBS of
                Right req -> do
                  putStrLn $ "Parsed RunSolverRequest: " ++ show req
                  return (RequestRunSolver req)
                Left err3 -> do
                  putStrLn "Failed to parse request"
                  putStrLn $ "Error parsing as GetExampleProblemsRequest: " ++ err1
                  putStrLn $ "Error parsing as GetAllFormulaNodesRequest: " ++ err2
                  putStrLn $ "Error parsing as RunSolverRequest: " ++ err3
                  fail "Invalid request"

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding A.defaultOptions
