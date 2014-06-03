{-# LANGUAGE FlexibleContexts #-}
module Parser.Class where

import AST.Eiffel

import Parser.Basic
import Parser.Feature
import Parser.Typ

import Data.Either

import Text.Parsec
import Text.Parsec.ByteString

genericsP :: Parser [Generic]
genericsP = squares (sepBy genericP comma)

genericP :: Parser Generic
genericP = Generic `fmap` identifier

note :: Parser ()
note = reserved "note" >> many1 (noteItem) >> return ()

noteItem :: Parser ()
noteItem = identifier >> reservedOp ":" >> anyString >> return ()

invariants :: Parser [Clause Expr]
invariants = reserved "invariant" >> many clause

inherits :: Parser [Typ]
inherits = many inheritP

redefineP = do
  reserved "redefine"
  many identifier
  reserved "end"
  return ()

inheritP :: Parser Typ
inheritP = do
  reserved "inherit"
  t <- classTyp
  optional rename
  optional redefineP
  return t

rename :: Parser ()
rename = do
  reserved "rename"
  renameName `sepBy` comma
  reserved "end"
  
renameName :: Parser ()
renameName = do
  identifier
  reserved "as"
  identifier
  return ()
  
createsP :: Parser [String]
createsP = do
  reserved "create"
  many identifier

absClas :: Parser (body Expr) -> Parser (AbsClas body Expr)
absClas featureP = do
  optional note
  optional (reserved "deferred")
  reserved "class"
  name <- identifier
  gen  <- option [] genericsP
  pgs  <- option [] procGens
  pes  <- many proc
  is   <- inherits
  cs   <- option [] createsP
  (fs, ds) <- absFeatureSects featureP
  invs <- option [] invariants
  reserved "end" 
  return ( AbsClas 
           { className  = name
           , redefine   = Redefine []
           , currProc   = Dot
           , procGeneric = pgs
           , procExpr   = pes
           , generics   = gen 
           , inherit    = is
           , creates    = cs
           , attributes = ds
           , features   = fs
           , invnts     = invs
           }
         )

absFeatureSects :: Parser (body Expr) -> Parser ([AbsFeature body Expr], [Decl])
absFeatureSects = fmap (foldl f ([],[])) . many . absFeatureSect
    where f (fs, ds) (fs', ds') = (fs ++ fs', ds ++ ds')

absFeatureSect :: Parser (body Expr) -> Parser ([AbsFeature body Expr], [Decl])
absFeatureSect featureP = do
  reserved "feature"
  optional (braces identifier)
  fds <- absFeatureOrDecls featureP
  return $ partitionEithers fds

absFeatureOrDecls :: Parser (body Expr) 
                  -> Parser [Either (AbsFeature body Expr) Decl]
absFeatureOrDecls = many .  absFeatureOrDecl

absFeatureOrDecl :: Parser (body Expr) 
                 -> Parser (Either (AbsFeature body Expr) Decl)
absFeatureOrDecl fp = try onlyDecl <|> absArgFeature fp
-- the order of the above matters, it would be nice to eliminate that

onlyDecl :: Parser (Either a Decl)
onlyDecl = do
  d <- declEq
  functionIndicators
  return (Right d)

resrv :: Stream s m Char => String -> ParsecT s u m Char
resrv str = reserved str >> return 'a'

functionIndicators :: Parser ()
functionIndicators =   
    notFollowedBy (resrv "do" <|>
                   resrv "external" <|>
                   resrv "once" <|>
                   resrv "deferred" <|>
                   resrv "local" <|>
                   resrv "procs" <|>
                   resrv "require" <|> 
                   resrv "require-locks")

absArgFeature :: Parser (body Expr) 
              -> Parser (Either (AbsFeature body Expr) Decl)
absArgFeature = fmap Left . feature

clas :: Parser Clas
clas = absClas featureImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP = absClas (return EmptyBody)