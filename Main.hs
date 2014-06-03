{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char (toLower)

import System.Console.CmdArgs

import AST.Class

import Parser.Parser (parseClassFile)

import TypeCheck.Class

import Generate.DepGen
import Generate.CompilePipeline (compileIO)


main :: IO ()
main = do
  argu <- cmdArgs "Eiffel Little Bytecode Compiler" [mode compMode]
  let file = head . compFile $ argu
      outFileM = output argu

  parseClassFile file >>= either print (compile outFileM (isMain argu))

compMode :: Args
compMode = CompArgs
           {
             compFile = def &= args
           , output   = def &= explicit & flag "o"
           , isMain   = def &= explicit & flag "m" & flag "main"
           , isLink   = def &= explicit & flag "l" & flag "link"
           }
             
data Args 
    = CompArgs 
      {
        compFile :: [String]
      , output   :: String
      , isMain   :: Bool
      , isLink   :: Bool
      } 
    deriving (Show, Data, Typeable)

clasFile :: Clas -> String
clasFile = (++ ".bc") . map toLower . className

compile :: String -> Bool -> Clas -> IO ()
compile outFile genMain clas = do
    let outFile' = case outFile of
                    "" -> clasFile clas
                    str -> str

    envsE <- depGenInt (className clas)
    case envsE of
      Left e -> putStrLn "Dependency Error:" >> error (show e)
      Right envs ->
          case typeCheckClassM envs clas of
            Left  e -> putStrLn "TypeChecking Error:" >> error e
            Right c -> compileIO outFile' genMain c
