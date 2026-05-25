{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >" #-}

module Main (main) where

import BranchAndPrune.BranchAndPrune (Problem (..))
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPProblem)
import LPPaver2.ExampleProblems (exampleProblems)
import LPPaver2.Export ()
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
  requestResponse stateMVar conn

requestResponse :: MVar ServerState -> WS.Connection -> IO ()
requestResponse stateMVar conn = do
  putStrLn "waiting for message from client..."
  msg <- WS.receiveData conn :: IO Text
  putStrLn $ "Received message: " ++ T.unpack msg
  request <- parseRequest msg
  response <- modifyMVar stateMVar $ \state ->
    handleRequest state request
  let responseJSON = A.encode response
  putStrLn $ "Sending response: " ++ TL.unpack (TL.decodeUtf8 responseJSON)
  WS.sendTextData conn responseJSON

------------------------
--- Example problems ---
------------------------

instance IsRequestResponse GetExampleProblemsRequest where
  type ResponseType GetExampleProblemsRequest = ExampleProblemsResponse
  handleRequest state _ = do
    let problems = exampleProblems 0
    let scopes = map (\p -> p.scope) $ Map.elems problems
    let problemForms = map (\p -> p.constraint) $ Map.elems problems
    let newState = ServerState.addBoxes scopes $ ServerState.addForms problemForms state
    pure (newState, ExampleProblemsResponse {problems = problems, boxes = newState.boxes})

data GetExampleProblemsRequest = GetExampleProblemsRequest
  deriving (Generic, Show)

instance A.FromJSON GetExampleProblemsRequest where
  parseJSON = A.withObject "GetExampleProblemsRequest" $ \_ -> pure GetExampleProblemsRequest

data ExampleProblemsResponse = ExampleProblemsResponse
  { problems :: Map.Map String LPPProblem,
    boxes :: BoxStore
  }
  deriving (Generic)

instance A.ToJSON ExampleProblemsResponse where
  toEncoding = A.genericToEncoding A.defaultOptions

------------------------------------------------
--- Request/Response boilerplate and parsing ---
------------------------------------------------

-- TODO: add continnuation for further responses
class IsRequestResponse request where
  type ResponseType request
  handleRequest :: ServerState -> request -> IO (ServerState, ResponseType request)

data Request
  = RequestGetExampleProblems GetExampleProblemsRequest
  | RequestTODO
  deriving (Generic)

data Response
  = ResponseExampleProblems ExampleProblemsResponse
  | ResponseTODO
  deriving (Generic)

instance IsRequestResponse Request where
  type ResponseType Request = Response
  handleRequest state (RequestGetExampleProblems req) = do
    (newState, resp) <- handleRequest state req
    pure (newState, ResponseExampleProblems resp)
  handleRequest state RequestTODO =
    pure (state, ResponseTODO)

parseRequest :: Text -> IO Request
parseRequest msg =
  let msgBS = T.encodeUtf8 msg
   in case A.eitherDecodeStrict msgBS of
        Right req -> do
          putStrLn $ "Parsed GetExampleProblemsRequest: " ++ show req
          return (RequestGetExampleProblems req)
        Left _err1 -> do
          -- case A.eitherDecodeStrict msgBS of
          --   Right req -> do
          --     return RequestTODO
          --   Left err2 -> do
          putStrLn "Failed to parse request"
          fail "Invalid request"

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding A.defaultOptions
