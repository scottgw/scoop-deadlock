module Deadlock.Feature
    ( checkFeature
    , reconstructFeature
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
import Deadlock.Reconstruct
import Deadlock.Stmt

import Debug.Trace

checkFeature :: TFeature -> DeadFeature ()
checkFeature = overFeature stmt

reconstructFeature :: TFeature -> DeadFeature (OrderRel Proc, [Proc])
reconstructFeature f =
  liftM2 (,) (overFeature reconstructOrder f)
             (overFeature reconstructLocks f)

overFeature :: (TStmt -> DeadStmt a) -> TFeature -> DeadFeature a
overFeature f feat =
  local (updRel (addProcs procs) . updFeatName name) checkFeatContrs
  where
    name = featureName feat
    procs = featureProcs feat
    args = featureArgs feat
    argsLks  = featureEnsLk feat ++ argProcs args
    prs  = featureReqLk feat
    fImpl = featureImpl feat
    body  = featureBody fImpl

    getProcFromDecl (SubTop p) = p
    getProcFromDecl (CreateLessThan p _q) = p
    localProcs = map getProcFromDecl (featureLocalProcs fImpl)
    
    checkFeatContrs =
      localM (updRelM (addExprs (prs ++ lessDotExprs argsLks)) .
              updRel (\o -> foldr (\ p r -> add r p) o localProcs) .
              updLocks (++ localProcs ++ featureEnsLk feat))
        checkFeatBody

    checkFeatBody =
      let
          upd   = local (updLocals feat .
                         updArgs feat .
                         addResult feat)
          updM  = localM (updRelM (liftError .
                                   addDecls (featureLocalProcs fImpl)))
      in (upd . updM) (lift . f $ body)



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
