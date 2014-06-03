{-# LANGUAGE TypeSynonymInstances
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
  #-}

module Deadlock.Monad where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Map (Map)

import AST.Eiffel

import Deadlock.Error
import Deadlock.OrderRel

import Util.Monad

data DeadContext a = 
    DeadContext 
    { ctxRel   :: OrderRel a
    , ctxVars  :: Map String Typ
    , ctxRes   :: Typ
    , ctxPos   :: SourcePos
    , ctxEnv   :: Map String ClasInterface
    , ctxLocks :: [Proc]
    } deriving Show

type ProcDeadCtx = DeadContext Proc
type Dead = ReaderT ProcDeadCtx (ErrorT PosDeadError Identity)

instance HasClasEnv (DeadContext a) where
    classEnv = ctxEnv
instance ClassReader ProcDeadCtx Dead

type DeadReader = ReaderT ProcDeadCtx Identity
type DeadStmt = WriterT [PosDeadError] DeadReader
type DeadFeature = ErrorT PosDeadError DeadStmt

run :: Dead a -> ProcDeadCtx -> Either PosDeadError a
run m = runIdentity . runErrorT . runReaderT m

runFeature :: DeadFeature a -> ProcDeadCtx 
           -> (Either PosDeadError a, [PosDeadError])
runFeature m ctx = 
    let me = runErrorT m
        mw = runWriterT me
        mr = runReaderT mw ctx
    in runIdentity mr

throwPosError :: (MonadReader ProcDeadCtx m, MonadError (Pos e) m) =>
              e -> m a
throwPosError e = (flip attachPos e . ctxPos) `liftM` ask >>= throwError

guardThrow :: (MonadReader ProcDeadCtx m, MonadError (Pos e) m) 
              => Bool -> e -> m ()
guardThrow False = throwPosError
guardThrow _ = const (return ())
