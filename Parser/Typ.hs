module Parser.Typ where

import AST.Eiffel

import Parser.Basic
import Parser.Expr

import Text.Parsec
import Text.Parsec.ByteString

likeTyp :: Parser Typ
likeTyp = reserved "like" >> Like `fmap` (identifier <|> (reserved "Current" >> return "Current"))

intTyp :: Parser Typ
intTyp = reserved "INTEGER" >> return IntType

doubleTyp :: Parser Typ
doubleTyp = reserved "REAL" >> return DoubleType

boolTyp :: Parser Typ
boolTyp = reserved "BOOLEAN" >> return BoolType

classTyp :: Parser Typ
classTyp = do
  i  <- identifier
  gs <- option [] (squares (sepBy typ comma))
  return (ClassType i gs)

detTyp :: Parser Typ
detTyp = reserved "detachable" >> (likeTyp <|> baseTyp)

attTyp :: Parser Typ
attTyp = reserved "attached" >> (likeTyp <|> baseTyp)

typ :: Parser Typ
typ = detTyp <|> attTyp <|> likeTyp <|> sepTyp <|> baseTyp

baseTyp :: Parser Typ
baseTyp = intTyp <|> boolTyp <|> doubleTyp <|> classTyp

sepTyp :: Parser Typ
sepTyp = do
  reserved "separate"
  p   <- angles procGen
  ps  <- option [] procGens
  cn  <- identifier
  return $ Sep p ps cn

declEq :: Parser Decl
declEq = do
  d <- decl
  optional (reservedOp "=" >> expr)
  return d

decl :: Parser Decl
decl = do
  name <- identifier <?> "Declaration identifier"
  decl' name

decl' :: String -> Parser Decl
decl' varName = do
  reservedOp ":"        <?> "Declaration ':'"
  typeName <- typ       <?> "Declaration type"
  return $ Decl varName typeName

argumentList :: Parser [Decl]
argumentList = option [] (parens (decl `sepBy` semicolon))

dot :: Parser Proc
dot = reserved "dot" >> return Dot

procGen :: Parser Proc
procGen = dot <|> Proc `fmap` identifier

procGens :: Parser [Proc]
procGens = angles (sepBy procGen comma)

proc :: Parser ProcDecl
proc = do
  pg <- procGen
  reservedOp ":"
  subTop pg <|> lessProc pg

procExprs :: Parser [ProcExpr]
procExprs = angles (sepBy procExprP comma)

procExprP :: Parser ProcExpr
procExprP = do
  a <- procGen
  reservedOp "<"
  b <- procGen
  return (LessThan a b)

subTop :: Proc -> Parser ProcDecl
subTop pg = do
  reserved "top"
  return $ SubTop pg

lessProc :: Proc -> Parser ProcDecl
lessProc pg = reservedOp "<" >> fmap (CreateLessThan pg) procGen
