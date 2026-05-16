{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >" #-}

module Main (main) where

import BranchAndPrune.BranchAndPrune (Problem (..))
import Control.Concurrent (MVar, newMVar)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPProblem)
import LPPaver2.ExampleProblems (exampleProblems)
import LPPaver2.Export ()
import LPPaver2.RealConstraints.Boxes (BoxStore, Box(..))
import Network.WebSockets qualified as WS
import Prelude

type ServerState = ()

newServerState :: ServerState
newServerState = ()

main :: IO ()
main = do
  putStrLn "Starting LPPaver2 server."
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application _state pending = do
  conn <- WS.acceptRequest pending
  putStrLn "Client connected."
  -- withPingThread conn 30 (return ()) (forever (requestResponse conn))
  requestResponse conn

requestResponse :: WS.Connection -> IO ()
requestResponse conn = do
  putStrLn "waiting for message from client..."
  msg <- WS.receiveData conn :: IO Text
  putStrLn $ "Received message: " ++ T.unpack msg

  request <- parseRequest msg
  response <- handleRequest request
  let responseJSON = A.encode response
  putStrLn $ "Sending response: " ++ show responseJSON
  WS.sendTextData conn responseJSON

class IsRequestResponse request where
  type ResponseType request
  handleRequest :: request -> IO (ResponseType request) -- TODO: sequence of responses, add state

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
  handleRequest (RequestGetExampleProblems req) =
    ResponseExampleProblems <$> handleRequest req
  handleRequest RequestTODO =
    pure ResponseTODO

parseRequest :: Text -> IO Request
parseRequest msg =
  let msgBS = T.encodeUtf8 msg
   in case A.eitherDecodeStrict msgBS of
        Right req -> do
          putStrLn $ "Parsed GetExampleProblemsRequest: " ++ show req
          return (RequestGetExampleProblems req)
        Left err1 -> do
          -- case A.eitherDecodeStrict msgBS of
          --   Right req -> do
          --     return RequestTODO
          --   Left err2 -> do
          putStrLn "Failed to parse request"
          fail "Invalid request"

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding A.defaultOptions

instance IsRequestResponse GetExampleProblemsRequest where
  type ResponseType GetExampleProblemsRequest = ExampleProblemsResponse
  handleRequest _ = do
    let problems = exampleProblems 0
    let scopes = map (\p -> scope (p :: LPPProblem)) $ Map.elems problems
    let boxes = Map.fromList [(box.boxHash, box) | box <- scopes]
    pure $ ExampleProblemsResponse {problems = problems, boxes = boxes}

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
