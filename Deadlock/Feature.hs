module Deadlock.Feature
    ( checkFeature
    , addProcs
    , addDecls
    , liftError
    ) where

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.Map as M

import AST.Eiffel

import TypeCheck.TypedExpr

import Deadlock.Error
import Deadlock.Expr
import Deadlock.OrderRel
import Deadlock.Context
import Deadlock.Monad
import Deadlock.Instantiate
import Deadlock.Stmt

checkFeature :: TFeature -> DeadFeature ()
checkFeature f =
    local (updRel (addProcs $ featureProcs f) . updFeatName (featureName f))
              (checkFeatContrs f)

checkFeatContrs :: TFeature -> DeadFeature ()
checkFeatContrs f =
    let argsLks  = featureEnsLk f ++ argProcs (featureArgs f)
        prs  = featureReqLk f
    in 
      localM (updRelM (addExprs (prs ++ lessDotExprs argsLks)) <=< 
                      liftError . appendLocks argsLks)
             (checkFeatBody f)

checkFeatBody :: TFeature -> DeadFeature ()
checkFeatBody f =
    let fImpl = featureImpl f
        upd   = local (updLocals f . updArgs f . addResult f)
        updM  = localM (updRelM (liftError . addDecls (featureLocalProcs fImpl)))
    in (upd . updM) (lift . stmt . featureBody $ fImpl)

lessDotExprs :: [Proc] -> [ProcExpr]
lessDotExprs = map (flip LessThan Dot)

addProcs :: [Proc] -> ProcOrder -> ProcOrder
addProcs = flip (foldl add)

updLocals :: TFeature -> ProcDeadCtx -> ProcDeadCtx
updLocals = updVars . M.union . localMap

updArgs :: TFeature -> ProcDeadCtx -> ProcDeadCtx
updArgs = updVars . M.union . argMap

addResult :: AbsFeature body exp -> ProcDeadCtx -> ProcDeadCtx
addResult f ctx = ctx { ctxRes = featureResult f}

argProcs :: [Decl] -> [Proc]
argProcs = map (typProc . declType)

addExpr :: ProcOrder -> ProcExpr -> DeadFeature ProcOrder
addExpr r (LessThan a b) = do
  checkInScope r a
  checkInScope r b
  if less r b a && a /= b
     then throwPosError (OrderCycle a b r)
     else return (addLess r a b)

addExprs :: [ProcExpr] -> ProcOrder -> DeadFeature ProcOrder
addExprs = flip (foldM addExpr)

liftError :: Dead a -> DeadFeature a
liftError m = do
  ctx <- lift ask
  case run m ctx of
    Left  e -> throwError e
    Right r -> return r
