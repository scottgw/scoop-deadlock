module Parser.Parser where

import AST.Eiffel

import qualified Data.ByteString.Char8 as B (readFile)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)

import Parser.Class
import Parser.Feature
import Parser.Statement

import Text.Parsec
import Text.Parsec.ByteString

parseFeature :: ByteString -> Either ParseError Feature
parseFeature = parse (feature featureImplP) ""

parseStmt :: ByteString -> Either ParseError Stmt
parseStmt = parse stmt  ""

parseClass :: ByteString -> Either ParseError Clas
parseClass = parse clas ""

parseInterface :: ByteString -> Either ParseError ClasInterface
parseInterface = parse clasInterfaceP ""

parseClass' :: ByteString -> Clas
parseClass' = either (error . show) id . parseClass

parseFromName :: ClassName -> IO Clas
parseFromName cn = 
    either (error . show) return . parseClass =<< B.readFile (classNameFile cn)

classNameFile :: ClassName -> String
classNameFile cn = map toLower cn ++ ".e"

parseClassFile :: String -> IO (Either ParseError Clas)
parseClassFile = parseFromFile clas