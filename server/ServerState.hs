module ServerState
  ( RunID (..),
    RunInfo (..),
    ServerState (..),
    new,
    addBoxes,
    addForms,
  )
where

import Data.Aeson qualified as A
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPStep)
import LPPaver2.RealConstraints (Box (..), BoxStore, ExprStore, Form (..), FormStore)
import Prelude

data ServerState = ServerState
  { boxes :: BoxStore,
    exprs :: ExprStore,
    forms :: FormStore,
    runs :: Map.Map RunID RunInfo
  }

newtype RunID = RunID String
  deriving (Eq, Ord, Show, Generic)

instance A.FromJSON RunID where
  parseJSON = A.genericParseJSON A.defaultOptions

instance A.ToJSON RunID where
  toEncoding = A.genericToEncoding A.defaultOptions

data RunInfo = RunInfo
  { runID :: RunID,
    runSteps :: [LPPStep]
  }

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