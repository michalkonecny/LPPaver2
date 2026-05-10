{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >" #-}

module Main (main) where

import AERN2.MP (MPBall, mpBallP)
import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (MPAffine), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune (Result (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ForkUtils (MonadUnliftIOWithState (..))
import Control.Monad (unless, void)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO (withRunInIO))
import Control.Monad.Logger (LoggingT (LoggingT, runLoggingT), MonadLogger, runStdoutLoggingT)
import Control.Monad.State (MonadState (get, put), StateT (StateT), runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson qualified as A
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.Redis qualified as Redis
import GHC.Records
import LPPaver2.BranchAndPrune
import LPPaver2.Export ()
import LPPaver2.RealConstraints
import MixedTypesNumPrelude
import System.Environment (getArgs)
import LPPaver2.ExampleProblems (exampleProblems)

sampleMPBall :: MPBall
sampleMPBall = mpBallP (MP.prec 1000) 0

sampleMPAffine :: MPAffine
sampleMPAffine = MPAffine _conf (convertExactly 0) Map.empty
  where
    _conf :: MPAffineConfig
    _conf = MPAffineConfig {maxTerms = int 10, precision = 1000}

processArgs :: [String] -> (LPPProblem, Rational, Int, Bool)
processArgs [probS, epsS, giveUpAccuracyS, maxThreadsS, verboseS] =
  (prob, giveUpAccuracy, maxThreads, isVerbose)
  where
    prob = fromJust $ Map.lookup probS (exampleProblems eps)
    eps = toRational (read epsS :: Double)
    giveUpAccuracy = toRational (read giveUpAccuracyS :: Double)
    maxThreads = read maxThreadsS :: Int
    isVerbose = verboseS == "verbose"
processArgs _ =
  error
    $ "Failed to match args.  Expected args: arithmetic problem eps giveUpAccuracy maxThreads verbose/silent"
    ++ "\n Available arithmetics: IA, AA"
    ++ "\n Available problems: "
    ++ List.concatMap ("\n" ++) problemNames
  where
    problemNames = Map.keys $ exampleProblems 0.0

-- |
-- Example runs:
--
-- > time branch-and-prune-example IA transitivityEps 0.005 0.001 4 verbose +RTS -N4
--
-- > time branch-and-prune-example AA cubicReduction 0.001 0.01 4 silent +RTS -N4
main :: IO ()
main = do
  (arith : args) <- getArgs
  case arith of
    "IA" ->
      mainWithArgs sampleMPBall $ processArgs args
    "AA" ->
      mainWithArgs sampleMPAffine $ processArgs args
    _ ->
      error $ "unknown arithmetic: " ++ arith

data RedisDestination = RedisDestination
  { connection :: Redis.Connection,
    sessionKeyPrefix :: String
  }

defaultRedisDestination :: Redis.Connection -> RedisDestination
defaultRedisDestination redisConn =
  RedisDestination
    { connection = redisConn,
      sessionKeyPrefix = "lppaver2:default:"
    }

boxesListKey :: RedisDestination -> BSS.ByteString
boxesListKey = stringToBSS . (<> "boxes") . sessionKeyPrefix

exprsListKey :: RedisDestination -> BSS.ByteString
exprsListKey = stringToBSS . (<> "exprs") . sessionKeyPrefix

formsListKey :: RedisDestination -> BSS.ByteString
formsListKey = stringToBSS . (<> "forms") . sessionKeyPrefix

stepsListKey :: RedisDestination -> BSS.ByteString
stepsListKey = stringToBSS . (<> "steps") . sessionKeyPrefix

data LPPControlState = LPPControlState
  { redisDest :: RedisDestination,
    boxesStore :: BoxStore,
    exprsStore :: ExprStore,
    formsStore :: FormStore
  }

instance Semigroup LPPControlState where
  s1 <> s2 =
    LPPControlState
      { redisDest = s1.redisDest,
        boxesStore = s1.boxesStore `Map.union` s2.boxesStore,
        exprsStore = s1.exprsStore `Map.union` s2.exprsStore,
        formsStore = s1.formsStore `Map.union` s2.formsStore
      }

defaultLPPControlState :: Redis.Connection -> LPPControlState
defaultLPPControlState redisConn =
  LPPControlState
    { redisDest = defaultRedisDestination redisConn,
      boxesStore = Map.empty,
      exprsStore = Map.empty,
      formsStore = Map.empty
    }

initControl :: (MonadIO m) => m LPPControlState
initControl = liftIO $ do
  -- Initialize Redis connection
  connection <- Redis.checkedConnect Redis.defaultConnectInfo
  let ctrlState = defaultLPPControlState connection

  -- Clear any previous session data
  let keyBuilders = [boxesListKey, exprsListKey, formsListKey, stepsListKey]
  let keys = map (\f -> f ctrlState.redisDest) keyBuilders
  Redis.runRedis connection $ do
    void $ Redis.del keys
  pure ctrlState

stringToBSS :: String -> BSS.ByteString
stringToBSS = TE.encodeUtf8 . T.pack

instance
  (MonadIO m, MonadState LPPControlState m, A.ToJSON r) =>
  BP.CanControlSteps m (LPPStep r)
  where
  reportStep step = do
    let boxes = getStepBoxes step
    let exprs = getStepExprs step
    let forms = getStepForms step

    ctrlState <- get
    -- update Redis with new boxes, formulas, expressions and the step itself
    liftIO $ Redis.runRedis ctrlState.redisDest.connection $ do
      let newBoxes = Map.difference boxes ctrlState.boxesStore
      updateRedisHashStore (boxesListKey ctrlState.redisDest) (Map.mapKeys unBoxHash newBoxes)

      let newExprs = Map.difference exprs ctrlState.exprsStore
      updateRedisHashStore (exprsListKey ctrlState.redisDest) (Map.mapKeys unExprHash newExprs)

      let newForms = Map.difference forms ctrlState.formsStore
      updateRedisHashStore (formsListKey ctrlState.redisDest) (Map.mapKeys unFormHash newForms)

      -- Push the step JSON to the Redis list of steps
      let stepJSONBSS = BSL.toStrict $ A.encode step
      void $ Redis.rpush (stepsListKey ctrlState.redisDest) [stepJSONBSS]

    -- update the control state with the new boxes, formulas and expressions
    let boxesStore = Map.union boxes ctrlState.boxesStore
    let exprsStore = Map.union exprs ctrlState.exprsStore
    let formsStore = Map.union forms ctrlState.formsStore
    put (ctrlState {boxesStore, exprsStore, formsStore})

updateRedisHashStore :: (A.ToJSON a) => BSS.ByteString -> Map.Map Int a -> Redis.Redis ()
updateRedisHashStore key store =
  unless (Map.null store) $ do
    let entries =
          [ (stringToBSS (show boxHash), BSL.toStrict $ A.encode box)
            | (boxHash, box) <- Map.toList store
          ]
    _ <- Redis.hmset key entries
    pure ()

instance (MonadUnliftIO m, Semigroup s) => MonadUnliftIOWithState (StateT s m) where
  type MonadUnliftIOState (StateT s m) = s
  toIOWithState mb = StateT $ \s -> do
    withRunInIO $ \runInIO -> do
      let ioWithS = runInIO (mb `runStateT` s)
      pure (ioWithS, s)
  absorbState s = StateT $ \s' -> pure ((), s <> s')

instance (Monad m, MonadUnliftIOWithState m) => MonadUnliftIOWithState (LoggingT m) where
  type MonadUnliftIOState (LoggingT m) = MonadUnliftIOState m
  toIOWithState lmb = LoggingT $ \loggerFn -> toIOWithState (runLoggingT lmb loggerFn)
  absorbState s = lift $ absorbState s

mainWithArgs ::
  (CanEval r, HasKleeneanComparison r, A.ToJSON r) =>
  r ->
  (LPPProblem, Rational, Int, Bool) ->
  IO ()
mainWithArgs sampleR (problem, giveUpAccuracy, maxThreads, isVerbose) = do
  ctrlState <- initControl
  _ <- runStateT (runStdoutLoggingT task) ctrlState
  pure ()
  where
    task :: (MonadLogger m, MonadIO m, MonadUnliftIOWithState m, MonadState LPPControlState m) => m ()
    task = do
      (Result paving _) <-
        lppBranchAndPrune sampleR
          $ LPPBPParams
            { maxThreads,
              giveUpAccuracy = giveUpAccuracy,
              problem,
              shouldLog = isVerbose
            }
      liftIO $ putStrLn $ BP.showPavingSummary paving
