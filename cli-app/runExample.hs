{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >" #-}

module Main (main) where

import AERN2.MP (MPBall, mpBallP)
import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (MPAffine), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune (Problem (..), Result (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Data.Aeson qualified as A
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.Redis qualified as Redis
import LPPaver2.BranchAndPrune
import LPPaver2.Export ()
import LPPaver2.RealConstraints
import MixedTypesNumPrelude
import System.Environment (getArgs)

problems :: Rational -> Map.Map String LPPProblem
problems eps =
  Map.fromList
    [ ( "transitivityEps",
        Problem
          { scope = mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0)), ("z", (0.0, 2.0))],
            constraint = (((x + eps) <= y) && (y <= z)) `formImpl` (x <= z)
          }
      ),
      ( "simpleAnd",
        Problem
          { scope = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 2.0))],
            constraint = (y <= 1 + eps) && (1 - eps <= y)
          }
      ),
      ( "circleEps",
        Problem
          { scope = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))],
            constraint = (x * x + y * y <= 1.0) `formImpl` (x * x + y * y <= 1.0 + eps)
          }
      ),
      ( "circleEpsSqrt",
        Problem
          { scope = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))],
            constraint = (sqrt (x * x + y * y) <= 1.0) `formImpl` (sqrt (x * x + y * y) <= 1.0 + eps)
          }
      ),
      ( "quadraticReduction",
        Problem
          { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
            constraint = 2.0 * x * x - 4.0 * x + 2.0 + y <= (-4.0) * (x - 1.0) + y
          }
      ),
      ( "cubicReduction",
        Problem
          { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
            constraint = 6.0 * x * x * x + x * x - 10.0 * x + 3.0 + y <= (x - 1.0) * (x - 4.5) + y + eps
          }
      ),
      ( "vcApproxSinLE",
        Problem
          { scope = mkBox [("r1", ((-3819831) / 4194304, 7639661 / 8388608)), ("x", ((-6851933) / 8388608, 6851933 / 8388608))],
            constraint =
              let t =
                    ( ( x
                          * ( ( ( ( (((-3350387) / 17179869184) * (x * x))
                                      + (4473217 / 536870912)
                                  )
                                    * (x * x)
                                )
                                  + ((-349525) / 2097152)
                              )
                                * (x * x)
                            )
                      )
                        + x
                    )
               in ( if x <= 1 / 67108864 && -x <= 1 / 67108864
                      then r1 == x
                      else
                        (r1 <= t + (4498891 / 100000000000000))
                          && ((t - (4498891 / 100000000000000)) <= r1)
                  )
                    && not ((r1 + ((-1.0) * sin x)) <= (58 * (1 / 1000000000)) + eps)
          }
      )
    ]
  where
    x = exprVar "x" :: Expr
    y = exprVar "y" :: Expr
    z = exprVar "z" :: Expr
    r1 = exprVar "r1" :: Expr

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
    prob = fromJust $ Map.lookup probS (problems eps)
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
    problemNames = Map.keys $ problems 0.0

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

-- do-nothing trivial step reporting
instance (MonadIO m) => BP.CanInitControl m where
  type ControlResources m = RedisDestination
  initControl = liftIO $ do
    -- Initialize Redis connection
    connection <- Redis.checkedConnect Redis.defaultConnectInfo
    let sessionKeyPrefix = "LPPaver2:default:"
    -- Clear any previous session data
    Redis.runRedis connection $ do
      _ <- Redis.del [stringToBSS $ sessionKeyPrefix <> "boxes"]
      _ <- Redis.del [stringToBSS $ sessionKeyPrefix <> "exprs"]
      _ <- Redis.del [stringToBSS $ sessionKeyPrefix <> "forms"]
      _ <- Redis.del [stringToBSS $ sessionKeyPrefix <> "steps"]
      pure ()
    pure $ RedisDestination {connection, sessionKeyPrefix}
  finaliseControl _ = pure ()

stringToBSS :: String -> BSS.ByteString
stringToBSS = TE.encodeUtf8 . T.pack

instance (MonadIO m) => BP.CanControlSteps m LPPStep where
  reportStep (RedisDestination {connection, sessionKeyPrefix}) step =
    liftIO $ Redis.runRedis connection $ do
      let stepsListKey = stringToBSS $ sessionKeyPrefix <> "steps"
      -- Extract boxes, formulas and expressions from the step and write them to Redis hashes
      -- TODO

      -- Push the step JSON to the Redis list of steps
      let stepJSONBSS = BSL.toStrict $ A.encode step
      _ <- Redis.rpush stepsListKey [stepJSONBSS]
      pure ()

mainWithArgs ::
  (CanEval r, HasKleenanComparison r) =>
  r ->
  (LPPProblem, Rational, Int, Bool) ->
  IO ()
mainWithArgs sampleR (problem, giveUpAccuracy, maxThreads, isVerbose) =
  runStdoutLoggingT task
  where
    task :: (MonadLogger m, MonadUnliftIO m) => m ()
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
