{-# LANGUAGE FlexibleContexts #-}
module Parser.Expr where

import Control.Applicative (liftA2)

import AST.Eiffel

import Parser.Basic

import Text.Parsec
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.ByteString

expr :: Parser Expr
expr = buildExpressionParser table factor

table :: Stream s m Char => OperatorTable s u m Expr
table = 
    [
     [prefix "not" (UnOpExpr Not)
     ,prefix "-"   (UnOpExpr Neg)
     ,prefix "sqrt" (UnOpExpr Sqrt)
     ]
    ,[binary "*"  (BinOpExpr Mul) AssocLeft
     ,binary "/"  (BinOpExpr Div) AssocLeft]
    ,[binary "+"  (BinOpExpr Add) AssocLeft
     ,binary "-"  (BinOpExpr Sub) AssocLeft]
    ,[binary "<=" (BinOpExpr (RelOp Lte NoType)) AssocLeft]
    ,[binary "<"  (BinOpExpr (RelOp Lt  NoType)) AssocLeft]
    ,[binary "="  (BinOpExpr (RelOp Eq  NoType)) AssocLeft]
    ,[binary "~"  (BinOpExpr (RelOp TildeEq  NoType)) AssocLeft]
    ,[binary "/=" (BinOpExpr (RelOp Neq NoType)) AssocLeft]
    ,[binary ">"  (BinOpExpr (RelOp Gt  NoType)) AssocLeft]
    ,[binary ">=" (BinOpExpr (RelOp Gte NoType)) AssocLeft]

    ,[
      binary "and then"  (BinOpExpr Or)   AssocLeft             
     ,binary "and"  (BinOpExpr And)  AssocLeft
     ,binary "or else"  (BinOpExpr Or)   AssocLeft
     ,binary "or"  (BinOpExpr Or)   AssocLeft
     ,binary "implies"  (BinOpExpr Implies)   AssocLeft
     ]
    ]

prefix :: Stream s m Char => 
          String -> (Expr -> UnPosExpr) -> Operator s u m Expr
prefix name fun = 
    Prefix (do
             p <- getPosition
             reservedOp name
             return (\ a -> attachPos p (fun a))
           )

binary :: Stream s m Char => 
          String -> (Expr -> Expr -> UnPosExpr) -> Assoc -> Operator s u m Expr
binary name fun = 
    Infix (do
            p <- getPosition
            reservedOp name
            return (\ a b -> attachPos p (fun a b))
          )

factor :: Parser Expr
factor = attachPosBefore factorUnPos

factorUnPos :: Parser UnPosExpr
factorUnPos = 
      try doubleLit
  <|> intLit
  <|> boolLit
  <|> stringLit
  <|> charLit
  <|> attached
  <|> try call
  <|> var
  <|> void
  <|> fmap contents (parens expr)

escChar, singQuote :: Parser String
escChar = string "%"
singQuote = string "\'"

charLit :: Parser UnPosExpr
charLit = do
  singQuote
  optional escChar
  c <- anyChar
  singQuote
  return (LitChar c)

stringLit :: Parser UnPosExpr
stringLit = LitString `fmap` anyString

attached :: Parser UnPosExpr
attached = do
  reserved "attached"
  cname <- braces identifier
  trg <- expr
  reserved "as"
  newName <- identifier
  return $ Attached cname trg newName

void :: Parser UnPosExpr
void = reserved "Void" >> return LitVoid

argsP = parens (expr `sepBy` comma)

varAttrCall = do
  i <- identifier
  notFollowedBy argsP
  return (VarOrCall i)

call = try qualCall <|> try unqualCall <|> varAttrCall

var :: Parser UnPosExpr
var = currentVar <|> resultVar <|> varAttrCall

unqualCall :: Parser UnPosExpr
unqualCall = liftA2 UnqualCall identifier argsP

qualCall :: Parser UnPosExpr
qualCall = attachPosBefore (var <|> unqualCall) >>= qualCall'

qualCall' :: Expr -> Parser UnPosExpr
qualCall' target = do
  reservedOp "."  
  i    <- identifier
  args <- option [] argsP
  let newTarget = attachPos (position target) (QualCall target i args)
  option (contents newTarget) (qualCall' newTarget)

resultVar :: Parser UnPosExpr
resultVar = reserved "Result" >> return ResultVar

currentVar :: Parser UnPosExpr
currentVar = reserved "Current" >> return CurrentVar

intLit :: Parser UnPosExpr
intLit = fmap (LitInt . fromIntegral) integer

doubleLit :: Parser UnPosExpr
doubleLit = fmap LitDouble float

boolLit :: Parser UnPosExpr
boolLit = (reservedOp "True" >> return (LitBool True)) <|>
          (reservedOp "False" >> return (LitBool False))