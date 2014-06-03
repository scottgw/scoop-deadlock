module Deadlock.Stmt (stmt) where

import Control.Monad.Reader
import Control.Monad.Writer

import Deadlock.Expr

import AST.Position
import AST.Typ
import AST.Stmt

import TypeCheck.TypedExpr 

import Deadlock.Error
import Deadlock.Monad

stmt :: TStmt -> DeadStmt ()
stmt = unposStmt . contents

unposStmt :: UnPosTStmt -> DeadStmt ()
unposStmt (Block s)       = mapM_ stmt s
unposStmt (CallStmt e)    = runE (expr e)
unposStmt (If b s1 s2)    = runE (expr b) >> stmt s1 >> stmt s2
unposStmt (Loop fr un lo) = stmt fr >> runE (expr un) >> stmt lo
unposStmt (Assign _ e)    = runE (expr e)
unposStmt (Print e)       = runE (expr e)
unposStmt (Malloc _)      = return ()
unposStmt (Create t f as) = runE (unposExpr (Call t f as NoType))
unposStmt BuiltIn         = return ()
unposStmt s = error $ "unposStmt: unhandled case -- " ++ show s

runE :: Dead a -> DeadStmt ()
runE = runAndAddErrors

runAndAddErrors :: Dead a -> DeadStmt ()
runAndAddErrors = runFromReport >=> addErrors

runFromReport :: Dead a -> DeadStmt (Either PosDeadError a)
runFromReport d = run d `fmap` lift ask

addErrors :: Either PosDeadError a -> DeadStmt ()
addErrors (Left e) = censor (e:) (return ())
addErrors _        = return ()