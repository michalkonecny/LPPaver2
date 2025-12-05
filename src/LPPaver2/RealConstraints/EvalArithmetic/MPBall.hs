module LPPaver2.RealConstraints.EvalArithmetic.MPBall () where

import AERN2.MP (MPBall)
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints.Boxes (Box (..), Box_ (..))
import LPPaver2.RealConstraints.Eval (CanGetVarDomain (..))
import LPPaver2.RealConstraints.Expr (Var)
import Text.Printf (printf)
import Prelude

boxGetVarDomain :: Box -> Var -> MPBall
boxGetVarDomain box var =
  case Map.lookup var box.box_.varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show box.box_.varDomains)
    Just dom -> dom

instance CanGetVarDomain MPBall where
  getVarDomain _sampleMPBall = boxGetVarDomain
