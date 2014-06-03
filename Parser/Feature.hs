{-# LANGUAGE KindSignatures #-}

module Parser.Feature where

import AST.Eiffel

import Parser.Basic
import Parser.Expr
import Parser.Statement
import Parser.Typ

import Text.Parsec
import Text.Parsec.ByteString

type FeatParser (body :: * -> *) exp = 
    Parser (body exp) -> Parser (AbsFeature body exp)

feature :: FeatParser body Expr
feature implP = do
  fr    <- option False (reserved "frozen" >> return True)
  name  <- identifier   <?> "Feature declaration identifier"
  args  <- argumentList <?> "Argument list"
  res   <- option NoType (reservedOp ":" >> typ)

  pGens <- option [] procGens

  reqLk <- option [] requireLocks
  reqs  <- option [] requires

  impl  <- implP
  
  ensLk <- option [] ensureLocks
  ens   <- option [] ensures

  reserved "end"

  return $ AbsFeature
             {
               featureFroz = fr
             , featureName = name
             , featureArgs   = args
             , featureResult = res
             , featureProcs  = pGens
             , featureReq    = reqs
             , featureReqLk  = reqLk

             , featureImpl   = impl
             , featureEns    = ens
             , featureEnsLk  = ensLk
             }

clause :: Parser (Clause Expr)
clause = do 
  tag <- identifier
  reservedOp ":"
  Clause tag `fmap` expr

obsolete :: Parser String
obsolete = reserved "obsolete" >> anyString

requires :: Parser [Clause Expr]
requires = reserved "require" >> many clause

ensures :: Parser [Clause Expr]
ensures = reserved "ensure" >> many clause

requireLocks :: Parser [ProcExpr]
requireLocks = reserved "require-locks" >> procExprs

ensureLocks :: Parser [Proc]
ensureLocks = reserved "ensure-locks" >> procGens

external :: Parser Stmt
external = attachPosBefore 
           (do
             reserved "external"
             s <- stringLiteral
             if s == "built_in" 
               then return BuiltIn 
               else parserFail "only supporting built_in external for now"
           )


deferred = attachPosBefore $ do
  reserved "deferred"
  return BuiltIn

featureImplP :: Parser (FeatureBody Expr)
featureImplP = do
  optional (reserved "is")
  optional obsolete
  procs <- option [] (reserved "procs" >> many proc)
  decls <- option [] (reserved "local" >> many decl)
  body  <- external <|> deferred <|> featBody
  return (FeatureBody
          { featureLocal = decls
          , featureLocalProcs = procs
          , featureBody  = body
          }
         )

featBody :: Parser Stmt 
featBody = attachPosBefore $
           (reserved "do" <|> reserved "once") >> 
           Block `fmap` stmts
