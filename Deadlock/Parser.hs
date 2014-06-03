module Deadlock.Parser
    (TestPlan (..)
    ,Test (..)
    ,parseTestPlan
    ) where

import Control.Monad

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.ParserCombinators.Parsec.Token as T

import Deadlock.Error
import Deadlock.OrderRel

import AST.Typ

identifier :: Parser String
identifier = T.identifier L.haskell

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp L.haskell

reserved :: String -> Parser ()
reserved   = T.reserved L.haskell

parseTestPlan :: FilePath -> IO TestPlan
parseTestPlan = fmap (either (error . show) id) . parseFromFile testPlan

data TestPlan = TestPlan [Test] deriving Show
data Test = Test 
    {testName :: String
    ,expectedResult :: Maybe [DeadError]
    } deriving Show

testPlan :: Parser TestPlan
testPlan = TestPlan `fmap` endBy test (reservedOp ";")

test :: Parser Test
test = do
  ident <- identifier
  reservedOp ":"
  s <- status
  return (Test ident s)

status :: Parser (Maybe [DeadError])
status = success <|> failure

success :: Parser (Maybe [DeadError])
success = reserved "Success" >> return Nothing

failure :: Parser (Maybe [DeadError])
failure = reserved "Failure" >> reserved "->" >> Just `fmap` errorTags

errorTags :: Parser [DeadError]
errorTags =  errorTag `sepBy` reservedOp ","

errorTag :: Parser DeadError
errorTag = lockVio <|> procScope <|> orderCycle <|> choice errorParsers

errorParsers :: [Parser DeadError]
errorParsers = map mkErrorParser errors

mkErrorParser :: DeadError -> Parser DeadError
mkErrorParser e = reserved (errorStr e) >> return e

proc = fmap Proc identifier

procScope :: Parser DeadError
procScope = reserved "ProcNotInScope" >> 
            fmap ProcNotInScope proc

lockVio :: Parser DeadError
lockVio = reserved "LockViolated" >> fmap (LockViolated . Proc) identifier

orderCycle :: Parser DeadError
orderCycle = reserved "OrderCycle" >>
             return (let noProc = Proc "noProc" in
                      OrderCycle noProc noProc (emptyRel noProc noProc))

errors :: [DeadError]
errors = [ArgViolation
         ,PostcondSubset
         ,PostcondArgs
         ]
