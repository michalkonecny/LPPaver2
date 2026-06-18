{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >" #-}

module Main (main) where

import BranchAndPrune.BranchAndPrune (Problem (..))
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Monad (forever)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), exampleProblems)
import LPPaver2.Export ()
import LPPaver2.RealConstraints (ExprStore, FormStore)
import LPPaver2.RealConstraints.Boxes (BoxStore)
import Network.WebSockets qualified as WS
import ServerState (ServerState (..))
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

data RunSolverRequest = RunSolverRequest
  { runId :: String,
    problemName :: String,
    paramValues :: Map.Map String Double,
    giveUpAccuracy :: Double,
    numberOfThreads :: Int
  }
  deriving (Generic, Show)

data SolverRunStatus = SolverRunning | SolverFinished | SolverFailed String
  deriving (Generic, Show)

data SolverRunStatusUpdate = SolverRunStatusUpdate
  { runId :: String,
    status :: SolverRunStatus
  }
  deriving (Generic)

instance IsRequestResponse RunSolverRequest where
  type ResponseType RunSolverRequest = SolverRunStatusUpdate
  handleRequest state req respond = do
    putStrLn $ "Received RunSolverRequest: " ++ show req
    respond (SolverRunStatusUpdate {runId = req.runId, status = SolverRunning})
    -- TODO
    pure state

instance A.FromJSON RunSolverRequest where
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
              putStrLn "Failed to parse request"
              putStrLn $ "Error parsing as GetExampleProblemsRequest: " ++ err1
              putStrLn $ "Error parsing as GetAllFormulaNodesRequest: " ++ err2
              fail "Invalid request"

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding A.defaultOptions
