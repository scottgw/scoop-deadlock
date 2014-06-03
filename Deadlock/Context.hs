{-# LANGUAGE FlexibleContexts #-}

module Deadlock.Context where

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (listToMaybe)

import Text.Parsec.Pos

import AST.Class
import AST.Decl
import AST.Feature
import AST.Typ

import Deadlock.Error
import Deadlock.Instantiate
import Deadlock.Monad
import Deadlock.OrderRel

import qualified Util.Monad as U

classToSep :: Typ -> Dead Typ
classToSep t@(ClassType cn _) = do
  c <- U.lookupClass t
  let ps = procGeneric c
      p = currProc c
  return (Sep p ps cn)
classToSep t = error $ "classToSep: can't make class " ++ show t ++ " separate"

appendLocks :: [Proc] -> ProcDeadCtx -> Dead ProcDeadCtx
appendLocks ls ctx =
    case outScope of
      []   -> return $ ctx {ctxLocks = (ctxLocks ctx) ++ ls}
      (v:_) -> throwPosError (ProcNotInScope v)
    where 
      outScope = filter (not . inDom r) ls
      r        = ctxRel ctx

updRel :: (ProcOrder -> ProcOrder) -> ProcDeadCtx -> ProcDeadCtx
updRel f ctx = ctx {ctxRel = f (ctxRel ctx)}

updRelM :: MonadReader ProcDeadCtx m => 
           (ProcOrder -> m ProcOrder) -> ProcDeadCtx -> m ProcDeadCtx
updRelM f ctx = do
  x <- f (ctxRel ctx)
  return (ctx {ctxRel = x})
  
updVars :: (Map String Typ -> Map String Typ) -> ProcDeadCtx -> ProcDeadCtx
updVars f ctx = ctx {ctxVars = f (ctxVars ctx)}

getRel :: Dead (ProcOrder)
getRel = fmap ctxRel ask

getLocks :: Dead [Proc]
getLocks = fmap ctxLocks ask

mkDeadCtx :: [ClasInterface] -> ProcOrder -> ProcDeadCtx
mkDeadCtx cis rel = 
    DeadContext 
    { ctxEnv   = clasMap cis
    , ctxVars  = M.empty
    , ctxRel   = rel
    , ctxPos   = initialPos "class"
    , ctxRes   = NoType
    , ctxLocks = [] -- took Dot out of here, but would be nice to leave it in,
                    -- and correct this through the reducedLocks function
    }

lookupClass :: Typ -> Dead ClasInterface
lookupClass t@(Sep _ _ cn) = do
  env <- ctxEnv `fmap` ask 
  case M.lookup cn env of
    Just c  -> instantiateSep t c
    Nothing -> error $ "lookupClass: tried to find " ++ cn ++ 
               " in " ++ show env
lookupClass t@(ClassType _ _) = classToSep t >>= lookupClass
lookupClass t = error $ "lookupClass: can't process " ++ show t

lookupVar :: String -> Dead Typ
lookupVar v = do
  vars <- ctxVars `fmap` ask
  case M.lookup v vars of
    Just t  -> return t
    Nothing -> 
        error $ "lookupVar: tried to find " ++ v ++ " in " ++ show vars

lookupFeature :: Typ -> String -> Dead FeatureI
lookupFeature t fName = do
  ci <- lookupClass t
  let f = listToMaybe . filter ( (== fName) . featureName) . features $ ci
  maybe (throwPosError (strMsg "can't find class")) return f

startingRel :: ProcOrder
startingRel = addLesses (emptyRel (Proc "top") (Proc "bot")) []

addDecls :: [ProcDecl] -> ProcOrder -> Dead ProcOrder
addDecls = flip (foldM addDecl)

addDecl :: (MonadError PosDeadError m, MonadReader ProcDeadCtx m)
           => ProcOrder -> ProcDecl -> m ProcOrder
addDecl r (SubTop p)           = return (add r p)
addDecl r (CreateLessThan p q) = do
  checkInScope r q
  if less r q p && q /= p
    then throwPosError (OrderCycle p q r)
    else return (addLess r p q)

checkInScope :: (MonadError PosDeadError m, MonadReader ProcDeadCtx m) 
                => ProcOrder -> Proc -> m ()
checkInScope r p
  | inDom r p = return () 
  | otherwise = throwPosError (ProcNotInScope p)