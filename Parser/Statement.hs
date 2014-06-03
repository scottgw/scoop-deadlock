{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Statement where

import AST.Eiffel

import Parser.Basic
import Parser.Expr

import Text.Parsec
import Text.Parsec.ByteString

stmt :: Parser Stmt
stmt = bareStmt >>= attachPosHere

bareStmt :: Parser UnPosStmt
bareStmt = assign <|> create <|> ifStmt <|> printStmt <|> printD <|> 
           loop <|> callStmt

stmts :: Parser [Stmt]
stmts = many stmt

block :: Parser UnPosStmt
block = fmap Block stmts

ifStmt :: Parser UnPosStmt
ifStmt = do
  b  <- reserved "if"   >> expr
  s1 <- reserved "then" >> fmap Block stmts >>= attachPosHere
  s2 <- option (Block []) elsePart >>= attachPosHere
  reserved "end"
  return (If b s1 s2)

elsePart :: Parser UnPosStmt
elsePart = ifelseP <|> elseP

elseP :: Parser UnPosStmt
elseP = reserved "else">> fmap Block stmts

ifelseP :: Parser UnPosStmt
ifelseP = do
  b <- reserved "elseif" >> expr
  s1 <- reserved "then" >> fmap Block stmts >>= attachPosHere
  s2 <- option (Block []) elsePart >>= attachPosHere
  return (If b s1 s2)

create :: Parser UnPosStmt
create = do
  reserved "create"
  v <- attachPosBefore var
  s <- (do
         reservedOp "."
         callE <- call
         case callE of
           UnqualCall fName args -> return (Create v fName args)
           VarOrCall fName -> return (Create v fName [])
       ) <|> return (DefCreate v)
  eol
  return s

loop :: Parser UnPosStmt
loop = do
  fr <- reserved "from"  >> block >>= attachPosHere
  un <- reserved "until" >> expr
  lo <- reserved "loop"  >> block >>= attachPosHere
  reserved "end"
  return (Loop fr un lo)

assignId :: Parser Expr
assignId = do
  i <- attachPosBefore var
  reservedOp ":="
  return i

callStmt :: Parser UnPosStmt
callStmt = do
  c <- attachPosBefore call
  eol
  return $ CallStmt c

assign :: Parser UnPosStmt
assign = do
  i <- try assignId
  e <- expr <?> "assignment expression"
  eol
  return $ Assign i e

printStmt :: Parser UnPosStmt
printStmt = do
  reserved "print"
  e <- parens expr
  eol
  return (Print e)

printD :: Parser UnPosStmt
printD = do
  reserved "printd"
  e <- parens expr
  eol
  return (PrintD e)