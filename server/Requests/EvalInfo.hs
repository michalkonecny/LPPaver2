module Requests.EvalInfo
  ( GetEvalInfoRequest (..),
    EvalInfoResponse (..),
    ProblemRef (..),
  )
where

import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.Export ()
import LPPaver2.RealConstraints (BoxHash (..), EvaluatedForm, FormHash (..))
import RequestHandler (IsRequestResponse (..), RequestHandlerInfo (..), aesonOptions)
import ServerState (RunID (..))
import ServerState qualified
import Prelude

-- | A problem identified by the hashes of its scope and constraint, as problems are sent to clients.
data ProblemRef = ProblemRef
  { scope :: BoxHash,
    constraint :: FormHash
  }
  deriving (Generic)

instance Show ProblemRef where
  show (ProblemRef {scope = BoxHash scopeH, constraint = FormHash constraintH}) =
    "ProblemRef {scope = " ++ show scopeH ++ ", constraint = " ++ show constraintH ++ "}"

-- | Asks for the evaluation info of the progress step that solved the given problem in the given run.
data GetEvalInfoRequest = GetEvalInfoRequest
  { runId :: RunID,
    problem :: ProblemRef
  }
  deriving (Generic, Show)

data EvalInfoResponse = EvalInfoResponse
  { runId :: RunID,
    problem :: ProblemRef,
    -- | Nothing if the run has no progress step for the problem.
    evalInfo :: Maybe EvaluatedForm
  }
  deriving (Generic)

instance IsRequestResponse GetEvalInfoRequest where
  type ResponseType GetEvalInfoRequest = EvalInfoResponse
  handleRequest RequestHandlerInfo {request, stateOnRequest, respond} =
    respond $ EvalInfoResponse {runId, problem, evalInfo}
    where
      runId = request.runId
      problem = request.problem
      evalInfo = ServerState.lookupEvalInfo runId (problem.scope, problem.constraint) stateOnRequest

-- | Hashes are sent to clients as decimal strings (see LPPaver2.Export), this is the inverse.
parseHash :: T.Text -> A.Parser Int
parseHash t = case T.signed T.decimal t of
  Right (n, rest) | T.null rest -> pure n
  _ -> A.parseFail ("not a hash: " ++ T.unpack t)

instance A.FromJSON BoxHash where
  parseJSON = A.withText "BoxHash" (fmap BoxHash . parseHash)

instance A.FromJSON FormHash where
  parseJSON = A.withText "FormHash" (fmap FormHash . parseHash)

instance A.FromJSON ProblemRef where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON ProblemRef where
  toEncoding = A.genericToEncoding aesonOptions

instance A.FromJSON GetEvalInfoRequest where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON EvalInfoResponse where
  toEncoding = A.genericToEncoding aesonOptions
