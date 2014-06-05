module Deadlock.Expr
    ( localM
    , checkInScope
    , expr
    , unposExpr
    )
    where

import Control.Monad.Reader

import AST.Decl
import AST.Position
import AST.Typ
import AST.Feature

import TypeCheck.TypedExpr 

import Deadlock.OrderRel
import Deadlock.Context
import Deadlock.Error
import Deadlock.Monad
import Deadlock.Instantiate
import Deadlock.Util

import Debug.Trace

expr :: TExpr -> Dead Proc
expr te = local (\ ctx -> ctx {ctxPos = position te}) (unposExpr $ contents te)

unposExpr :: UnPosTExpr -> Dead Proc
unposExpr (Call trg fName args _) = do
  trgP <- expr trg -- FIXME: should we use the target proc to instantiate the feature?
  argsP <- mapM expr args
  feat  <- callWithArgs argsP `fmap` lookupFeature (texpr trg) fName   

  validPrecond (featureReqLk feat)
  validArgs trgP argsP
  validPostcond (featureEnsLk feat)

  return (typProc $ featureResult feat)
unposExpr (BinOpExpr _ e1 e2 _) = expr e1 >> expr e2 >> return Dot
unposExpr (UnOpExpr _ e _) = expr e
unposExpr (Access _trg _v t) = return (typProc t)
unposExpr (Var _ t)        = return (typProc t)
unposExpr (LitInt _)       = return Dot
unposExpr (LitDouble _)    = return Dot
unposExpr (LitBool _)      = return Dot
unposExpr (Cast t e)       = expr e >> return (typProc t)
unposExpr (CurrentVar _)   = return Dot
unposExpr (ResultVar _)    = (typProc . ctxRes) `fmap` ask
unposExpr (LitVoid t)      = return (typProc t)
unposExpr (LitString _)    = return Dot
unposExpr (LitChar _)      = return Dot
unposExpr s                = error $ "unposExpr:" ++ show s

validArgs :: Proc -> [Proc] -> Dead ()
validArgs trg args = do
  r <- getRel
  lks <- getLocks

  let
    checkArgLessTarget a = guardThrow (less r a trg) (ArgNotLessTarget a)
    checkInLocks a = guardThrow (a == Dot || a `elem` lks) (ArgNotInContextLocks a)

  mapM_ checkArgLessTarget args
  mapM_ checkInLocks args

validPrecond :: [ProcExpr] -> Dead ()
validPrecond precondExprs = do
  r <- getRel
  r' <- createExprOrder precondExprs startingRel
  guardThrow (r' `subOrder` r) (trace (show precondExprs) CallOrdNotInContextOrd r' r)

validPostcond :: [Proc] -> Dead ()
validPostcond postLks =
  do lks <- getLocks
     r <- getRel
     let
       checkLessAllLks l = guardThrow (all (less r l) lks) (CallLocksNotLessContextLocks l)
     mapM_ checkLessAllLks postLks

localM :: MonadReader r m => (r -> m r) -> m a -> m a
localM f m = do
  x <- ask >>= f
  local (const x) m
