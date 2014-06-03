module Deadlock.Expr
    ( localM
    , checkInScope
    , expr
    , unposExpr
    )
    where

import Control.Monad.Reader

import Data.List (nub)

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

expr :: TExpr -> Dead Proc
expr te = local (\ ctx -> ctx {ctxPos = position te}) (unposExpr $ contents te)

unposExpr :: UnPosTExpr -> Dead Proc
unposExpr (Call trg fName args _) = do
  trgP <- expr trg -- FIXME: should we use the target proc to instantiate the feature?
  argsP <- mapM expr args
  feat  <- callWithArgs argsP `fmap` lookupFeature (texpr trg) fName   

  validPrecond (featureReqLk feat)
  validArgs trgP argsP
  validPostcond argsP (featureEnsLk feat)

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
--  lks <- getLocks
  guardThrow (all (flip (less r) trg) args) ArgViolation
  -- we should also handle the case where an argument is a local processor
  -- declared in the surrounding function
--  guardThrow (allLessOne r args lks) ArgViolation 

validPrecond :: [ProcExpr] -> Dead ()
validPrecond precondExprs = do
  r <- getRel
  r' <- createExprOrder precondExprs startingRel
  guardThrow (r' `subOrder` r) (NonSubOrder r' r)

validPostcond :: [Proc] -> [Proc] -> Dead ()
validPostcond args postLks = join $
  validPostcond' args postLks `fmap` getRel `ap` getLocks

validPostcond' ::[Proc] -> [Proc] -> ProcOrder -> [Proc] -> Dead ()
validPostcond' args postLks r lks =
    guardThrow (null (locksTaken args) || postLks `allLess` lows) PostcondArgs >>
    guardThrow (null postLks || highs `allLess` lks) PostcondSubset
    where allLess these lessThanThese = all (lessOne' r lessThanThese) these
          lows  = lowest r args
          highs = highest r postLks
          locksTaken = filter (/= Dot)

lessOne' :: (Ord a, Show a) => OrderRel a -> [a] -> a -> Bool
lessOne' r = flip (lessOne r)

localM :: MonadReader r m => (r -> m r) -> m a -> m a
localM f m = do
  x <- ask >>= f
  local (const x) m

lowest, highest :: (Ord a, Show a) => OrderRel a -> [a] -> [a]
lowest  r ps = nub (filter (flip (lessOne r) ps) ps)
highest r ps = nub (filter (flip (greaterOne r) ps) ps)

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

procExprToDecl :: ProcExpr -> ProcDecl
procExprToDecl (LessThan a b) = CreateLessThan a b

createExprOrder :: [ProcExpr] -> ProcOrder -> Dead ProcOrder
createExprOrder es = addDecls (map procExprToDecl es) . 
                     flip (foldr addExpr) es

addExpr :: ProcExpr -> ProcOrder -> ProcOrder
addExpr (LessThan a b) = flip add a . flip add b
