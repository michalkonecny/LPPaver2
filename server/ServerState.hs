module ServerState (ServerState (..), newServerState, addBoxes) where

import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints (Box (..), BoxStore, ExprStore, FormStore)

data ServerState = ServerState
  { allBoxes :: BoxStore,
    exprs :: ExprStore,
    forms :: FormStore
  }

newServerState :: ServerState
newServerState =
  ServerState
    { allBoxes = Map.empty,
      exprs = Map.empty,
      forms = Map.empty
    }

addBoxes :: ServerState -> [Box] -> ServerState
addBoxes state newBoxes =
  state {allBoxes = state.allBoxes `Map.union` Map.fromList [(b.boxHash, b) | b <- newBoxes]}