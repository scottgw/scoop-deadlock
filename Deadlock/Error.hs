{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Deadlock.Error where

import Control.Monad.Error

import AST.Position
import AST.Typ

import Deadlock.OrderRel

import Text.Parsec.Error

class ErrorString a where
    errorStr :: a -> String

type ProcOrder = OrderRel Proc
type PosDeadError = Pos DeadError

parseErrorToDead :: ParseError -> PosDeadError
parseErrorToDead e = 
    attachPos (errorPos e) (DeadErrorString (show e))

data DeadError = LockViolated Proc
               | ProcNotInScope Proc
               | NonSubOrder ProcOrder ProcOrder
               | NoClassProc
               | InstProcsMismatch
               | OrderCycle Proc Proc ProcOrder
               | DeadErrorString String 
               | Precond
               | ArgViolation
               | PostcondSubset
               | PostcondArgs
               | NoError
                 deriving (Show, Eq)

instance ErrorString DeadError where
    errorStr = show

instance Error DeadError where
    strMsg = DeadErrorString

instance Error [PosDeadError] where
    strMsg = (:[]) . strMsg

instance Error PosDeadError


pretty (DeadErrorString s) = s
pretty e = show e
