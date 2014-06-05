module Deadlock.Util (callWithArgs, createExprOrder) where

import AST.Decl
import AST.Typ
import AST.Feature
import Deadlock.Instantiate
import Deadlock.Context
import Deadlock.Monad
import Deadlock.Error
import Deadlock.OrderRel

type MProcs = Maybe [Proc]

callWithArgs :: [Proc] -> FeatureI -> FeatureI
callWithArgs es fDecl = 
    matchMaybeArgs
      (map declProcs (featureArgs fDecl))
      es
      fDecl

matchMaybeArgs :: [MProcs] -> [Proc] -> FeatureI -> FeatureI
matchMaybeArgs es as = foldl (.) id (zipWith matchMaybeArg es as)

matchMaybeArg :: MProcs -> Proc -> FeatureI -> FeatureI
matchMaybeArg (Just (p1:_)) p2 = instFeat p1 p2
matchMaybeArg (Just []) _      = error "matchMaybeArg: unknown pattern"
matchMaybeArg Nothing _        = id

createExprOrder :: [ProcExpr] -> ProcOrder -> Dead ProcOrder
createExprOrder es = addDecls (map procExprToDecl es) . 
                     flip (foldr addExpr) es

procExprToDecl :: ProcExpr -> ProcDecl
procExprToDecl (LessThan a b) = CreateLessThan a b

addExpr :: ProcExpr -> ProcOrder -> ProcOrder
addExpr (LessThan a b) = flip add a . flip add b
