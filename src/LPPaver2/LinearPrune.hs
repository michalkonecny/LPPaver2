module LPPaver2.LinearPrune
  ( extractCIEorDIE,
    IEFormType (..),
  )
where

import GHC.Records (HasField (..))
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import Prelude qualified as P

data IEFormType
  = IE -- single inequality
  | CIE -- conjunction of inequalities
  | DIE -- disjunction of inequalities
  deriving (P.Eq, P.Show)

extractCIEorDIE :: Form -> P.Maybe (Form, IEFormType)
extractCIEorDIE form0 = extractH form0.root
  where
    extractH :: FormHash -> P.Maybe (Form, IEFormType)
    extractH formH =
      let form = form0 {root = formH}
       in case lookupFormNode form0 formH of
            FormComp {comp} ->
              case comp of
                CompLe -> Just (form, IE) -- could be part of a conjunction or disjunction
                CompLeq -> Just (form, IE) -- ditto
                CompEq -> Just (form, CIE) -- a == b  ~  (a <= b) && (a >= b)
                CompNeq -> Just (form, DIE) -- a != b  ~  (a < b) || (a > b)
            FormBinary {bconn, f1, f2} ->
              let f1Info = extractH f1
                  f2Info = extractH f2
               in case bconn of
                    ConnAnd ->
                      case (f1Info, f2Info) of
                        (Just (f1IE, t1), Just (f2IE, t2))
                          | t1 P./= DIE P.&& t2 P./= DIE -> Just (f1IE && f2IE, CIE) -- both compatible with CIE
                          | t1 P./= DIE -> Just (f1IE, CIE) -- only f1 compatible with CIE
                          | t2 P./= DIE -> Just (f2IE, CIE) -- only f2 compatible with CIE
                          | otherwise -> Nothing -- not a conjuction with inequalities
                        (Just (_, CIE), Nothing) -> f1Info
                        (Just (f1IE, IE), Nothing) -> Just (f1IE, CIE) -- conjuction, no longer IE
                        (Nothing, Just (_, CIE)) -> f2Info
                        (Nothing, Just (f2IE, IE)) -> Just (f2IE, CIE) -- conjuction, no longer IE
                        _ -> Nothing
                    ConnOr ->
                      case (f1Info, f2Info) of
                        (Just (f1IE, t1), Just (f2IE, t2))
                          | t1 P./= CIE P.&& t2 P./= CIE -> Just (f1IE || f2IE, DIE) -- both compatible with DIE
                          | t1 P./= CIE -> Just (f1IE, DIE) -- only f1 compatible with DIE
                          | t2 P./= CIE -> Just (f2IE, DIE) -- only f2 compatible with DIE
                          | otherwise -> Nothing -- not a disjunction with inequalities
                        (Just (_, DIE), Nothing) -> f1Info
                        (Just (f1IE, IE), Nothing) -> Just (f1IE, DIE) -- disjunction, no longer IE
                        (Nothing, Just (_, DIE)) -> f2Info
                        (Nothing, Just (f2IE, IE)) -> Just (f2IE, DIE) -- disjunction, no longer IE
                        _ -> Nothing
                    _ -> Nothing
            _ -> Nothing
