{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import qualified Control.Exception as Ex

import Text.PrettyPrint

import System.Environment
import System.FilePath
import System.Directory

import AST.Position

import Deadlock.Error
import Deadlock.Parser
import Deadlock.Class

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No argument given"
    (filePath:_) ->
      do let parts = splitPath filePath
             fileName = last parts
             path = joinPath (init parts)
         when (path /= "") (setCurrentDirectory path)
         testPlan <- parseTestPlan fileName
         runTests testPlan

runTests :: TestPlan -> IO ()
runTests (TestPlan ts) = mapM runTest ts >>= display

runTest :: Test -> IO (String, String)
runTest t@(Test name _) = Ex.catch (runTest' t) 
  (\e -> return (name, "RUNTIME ERROR: " ++ show (e :: Ex.SomeException)))

runTest' :: Test -> IO (String, String)
runTest' (Test name expected) = do
  results <- deadCheckFile (name ++ ".e")
  (name,) `fmap` response name expected results

-- | An error that doesn't want details about the order
-- but otherwise must match in the Eq instance.
newtype FuzzyError = FuzzyError {unFuzzyError :: DeadError}

instance Eq FuzzyError where
  FuzzyError (OrderCycle _ _ _) == FuzzyError (OrderCycle _ _ _) = True
  FuzzyError e1 == FuzzyError e2 = e1 == e2

response :: String -> Maybe [DeadError] -> Maybe [PosDeadError] -> IO String
response _ Nothing   Nothing = return "OK"
response _ (Just es) Nothing = return ("No error, was expecting " ++ show es)
response name Nothing (Just es) = reportFails name [] es
response name (Just es) (Just es')
    | map FuzzyError es == map (FuzzyError . contents) es' = return "OK"
    | otherwise = reportFails name es es'

reportFails :: String -> [DeadError] -> [PosDeadError] -> IO String
reportFails name expects = 
    fmap concat . zipWithM (reportFail name) (expects ++ repeat NoError)

reportFail :: String -> DeadError -> PosDeadError -> IO String
reportFail _ expect actual =  
    return $ concat ["expected: ", show expect
                    , " -- actual result: ", show actual]

display :: [(String, String)] -> IO ()
display ss = 
    let
        (ns, rs) = unzip ss
        len      = maximum (map length ns) + 1
        go n r   = text n $$ nest len (text "==>" <+> text r)
        result   = vcat $ zipWith go ns rs
    in 
      putStrLn (render result)
