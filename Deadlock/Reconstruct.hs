{-# LANGUAGE ScopedTypeVariables #-}
module Deadlock.Reconstruct (reconstructLocks,
                             reconstructOrder) where

import Control.Monad.Reader
import Control.Monad.Writer

import Data.List (nub)
import Data.Monoid

import AST.Position
import AST.Typ
import AST.Feature
import AST.Stmt

import TypeCheck.TypedExpr 

import Deadlock.Context
import Deadlock.OrderRel
import Deadlock.Error
import Deadlock.Monad
import Deadlock.Instantiate
import Deadlock.Util

newtype ProcOrderMonoid = ProcOrderMonoid {unProcOrderMonoid :: ProcOrder}

instance Monoid ProcOrderMonoid where
  mempty = ProcOrderMonoid $ emptyRel (Proc "top") (Proc "bot")
  mappend (ProcOrderMonoid o1) (ProcOrderMonoid o2) =
    ProcOrderMonoid (unionRel o1 o2)

reconstructLocks :: TStmt -> DeadStmt [Proc]
reconstructLocks s = nub `fmap` overStmt exprLks s

reconstructOrder :: TStmt -> DeadStmt ProcOrder
reconstructOrder s = unProcOrderMonoid `fmap` overStmt exprOrd s

overStmt :: Monoid a => (TExpr -> Dead a) -> TStmt -> DeadStmt a
overStmt f = unposStmt f . contents

unposStmt :: Monoid a => (TExpr -> Dead a) -> UnPosTStmt -> DeadStmt a
unposStmt f = go
  where
    go (Block s)       = mconcat `fmap` mapM stmt s
    go (CallStmt e)    = liftD (f e)
    go (If b s1 s2)    = liftD (f b) >> stmt s1 >> stmt s2
    go (Loop fr un lo) = stmt fr >> liftD (exprLks un) >> stmt lo
    go (Assign _ e)    = liftD (f e)
    go (Print e)       = liftD (f e)
    go (Malloc _)      = return mempty
    go (Create t name as) =
      do p <- ctxPos `fmap` ask
         n <- ctxFeatName `fmap` ask
         liftD (f $ attachPos n p (Call t name as NoType))
    go BuiltIn         = return mempty
    go s = error $ "unposStmt.go: unhandled case -- " ++ show s

    stmt = overStmt f

liftD :: Dead a -> DeadStmt a
liftD d =
  do Right s <- run d `fmap` lift ask
     return s


exprOrd :: TExpr -> Dead ProcOrderMonoid
exprOrd e' = snd `fmap` exprProcOrd e'
  where
    exprProcOrd :: TExpr -> Dead (Proc, ProcOrderMonoid)
    exprProcOrd = unposExpr . contents

    unposExpr :: UnPosTExpr -> Dead (Proc, ProcOrderMonoid)
    unposExpr (Call trg fName args _) = do
      (_trgP, trgOrd) <- exprProcOrd trg -- FIXME: should we use the target proc to instantiate the feature?

      argsProcsLocks <- mapM exprProcOrd args
      let (argsP, argsLocks) = unzip argsProcsLocks

      feat <- callWithArgs argsP `fmap` lookupFeature (texpr trg) fName

      callOrd <- createExprOrder (featureReqLk feat) (unProcOrderMonoid mempty)
      let callOrd' = ProcOrderMonoid callOrd

      return (typProc $ featureResult feat, trgOrd <> mconcat argsLocks <> callOrd')
    unposExpr (BinOpExpr _ e1 e2 _) =
      do (_, ord1) <- exprProcOrd e1
         (_, ord2) <- exprProcOrd e2
         return (Dot, ord1 <> ord2)
    unposExpr (UnOpExpr _ e _) = exprProcOrd e
    unposExpr (Cast t e)       =
      do (_p, ord) <- exprProcOrd e
         return (typProc t, ord)
    unposExpr (ResultVar _)    =
      do t <- ctxRes `fmap` ask
         return (typProc t, mempty)
    unposExpr (Access _trg _v t) = return (typProc t, mempty)
    unposExpr (Var _ t)        = return (typProc t, mempty)
    unposExpr (LitInt _)       = dotEmpty
    unposExpr (LitDouble _)    = dotEmpty
    unposExpr (LitBool _)      = dotEmpty
    unposExpr (CurrentVar _)   = dotEmpty
    unposExpr (LitVoid t)      = return (typProc t, mempty)
    unposExpr (LitString _)    = dotEmpty
    unposExpr (LitChar _)      = dotEmpty
    unposExpr s                = error $ "unposExpr:" ++ show s

exprLks :: TExpr -> Dead [Proc]
exprLks e' = snd `fmap` exprProcLks e'
  where
    exprProcLks :: TExpr -> Dead (Proc, [Proc])
    exprProcLks = unposExpr . contents

    unposExpr :: UnPosTExpr -> Dead (Proc, [Proc])
    unposExpr (Call trg fName args _) = do
      (_trgP, trgLks) <- exprProcLks trg -- FIXME: should we use the target proc to instantiate the feature?

      argsProcsLocks <- mapM exprProcLks args
      let (argsP, argsLocks) = unzip argsProcsLocks

      feat <- callWithArgs argsP `fmap` lookupFeature (texpr trg) fName

      return (typProc $ featureResult feat, trgLks ++ concat argsLocks ++ argsP)
    unposExpr (BinOpExpr _ e1 e2 _) =
      do (_, lks1) <- exprProcLks e1
         (_, lks2) <- exprProcLks e2
         return (Dot, lks1 ++ lks2)
    unposExpr (UnOpExpr _ e _) = exprProcLks e
    unposExpr (Cast t e)       =
      do (_p, lks) <- exprProcLks e
         return (typProc t, lks)
    unposExpr (ResultVar _)    =
      do t <- ctxRes `fmap` ask
         return (typProc t, [])
    unposExpr (Access _trg _v t) = return (typProc t, [])
    unposExpr (Var _ t)        = return (typProc t, [])
    unposExpr (LitInt _)       = dotEmpty
    unposExpr (LitDouble _)    = dotEmpty
    unposExpr (LitBool _)      = dotEmpty
    unposExpr (CurrentVar _)   = dotEmpty
    unposExpr (LitVoid t)      = return (typProc t, [])
    unposExpr (LitString _)    = dotEmpty
    unposExpr (LitChar _)      = dotEmpty
    unposExpr s                = error $ "unposExpr:" ++ show s

dotEmpty :: Monoid a => Dead (Proc, a)
dotEmpty = return (Dot, mempty)
