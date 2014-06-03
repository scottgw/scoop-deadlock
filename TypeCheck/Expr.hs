{-# LANGUAGE ViewPatterns #-}
module TypeCheck.Expr (clause, typeOfExpr, convertVarCall) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Data.Maybe

import AST.Eiffel

import qualified TypeCheck.TypedExpr as T
import TypeCheck.TypedExpr (TExpr)

import TypeCheck.BasicTypes
import TypeCheck.Context
import TypeCheck.Generic

import Util.Monad

clause :: Clause Expr -> Typing (Clause TExpr)
clause (Clause n e) = Clause n `fmap` typeOfExpr e

convertVarCall :: String -> TExpr -> Typing (Maybe TExpr)
convertVarCall vc trg = do
  trgM <- castTargetM trg vc
  p <- currentPos
  case trgM of
    Nothing -> 
        do
          vTyp <- typeOfVar vc
          return (attachPos p <$> (T.Var vc <$> vTyp))
    Just trg' -> compCall vc trg' p <$> resolveIFace (T.texpr trg')
           

compCall :: String -> TExpr -> SourcePos -> ClasInterface -> Maybe TExpr
compCall vc trg p ci = 
    attachPos p <$> (asAccess trg ci vc <|> 
                     asCall trg ci vc)

asAccess :: TExpr -> ClasInterface -> String -> Maybe T.UnPosTExpr
asAccess trg ci vc = 
    let dec = findAttrInt ci vc
    in liftA2 (T.Access trg) (declName <$> dec) (declType <$> dec)

asCall :: TExpr -> ClasInterface -> String -> Maybe T.UnPosTExpr
asCall trg ci vc =
  do f <- findFeatureInt ci vc
     guard (null (featureArgs f))
     return (T.Call trg (featureName f) [] (featureResult f))

typeOfExpr :: Expr -> Typing TExpr
typeOfExpr e = setPosition (position e) (expr (contents e))

binOpArgCasts :: TExpr -> TExpr -> Typing (TExpr, TExpr)
binOpArgCasts e1 e2 
    | isBasic t1 && isBasic t2 = return (e1,e2)
    | otherwise = 
        do
          cast1M <- conforms t1 t2
          case cast1M of
            Just cast1 -> return (cast1 e1, e2)
            Nothing -> 
                do
                  cast2M <- conforms t2 t1
                  case cast2M of
                    Just cast2 -> return (e1, cast2 e2)
                    Nothing -> throwError $ "binOpArgCasts: " ++ 
                               show (e1,e2)
    where 
      t1 =  T.texprTyp (contents e1)
      t2 =  T.texprTyp (contents e2)
                                       

expr :: UnPosExpr -> Typing TExpr
expr (VarOrCall s) = (convertVarCall s =<< currentM) >>=
    maybe (throwError ("Can't resolve " ++ s)) return
expr CurrentVar    = currentM
expr ResultVar     = (T.ResultVar <$> result <$> ask) >>= tagPos
expr (LitInt i)    = tagPos (T.LitInt i)
expr (LitDouble d) = tagPos (T.LitDouble d)
expr (LitBool b)   = tagPos (T.LitBool b)
expr (UnOpExpr op e) = tagPos =<< (T.UnOpExpr op <$> te <*> res)
  where te  = typeOfExpr e
        res = unOpTypes op =<< (T.texpr <$> te)
expr (BinOpExpr op e1 e2) = do
  e1' <- typeOfExpr e1
  e2' <- typeOfExpr e2
  (e1'', e2'') <- binOpArgCasts e1' e2'
  (resType, argType) <- opTypes op (T.texpr e1'') (T.texpr e2'')
  tagPos $ T.BinOpExpr 
         (castOp op argType)
         (castTyp argType e1'')
         (castTyp argType e2'')
         resType
expr (UnqualCall fName args) = 
  expr =<< QualCall <$> tagPos CurrentVar <*> pure fName <*> pure args
expr (QualCall trg fName args) = do 
  trgUncasted <- typeOfExpr trg
  tTrg <- castTarget trgUncasted fName
  let t        = T.texpr tTrg
      realArgs = castGenericArgsM t fName =<< mapM typeOfExpr args
      resultT  = featureResult <$> fName `inClass` t
      call     = (validCall t fName args) *>
                 (castResult t fName =<< tagPos =<< 
                  T.Call tTrg fName <$> realArgs <*> resultT)
  maybe call castAccess =<< convertVarCall fName tTrg
expr (Cast t e) = T.Cast t `fmap` typeOfExpr e >>= tagPos
expr LitVoid    = tagPos (T.LitVoid VoidType)
expr (LitString s) = tagPos (T.LitString s)
expr (LitChar c) = tagPos (T.LitChar c)
expr e          = error ("expr: unimplemented : " ++ show e)

validCall :: Typ -> String -> [Expr] -> Typing ()
validCall t fName args = join (argsConform <$> argTypes <*> formTypes)
    where instFDecl = fName `inClassGen` t
          formTypes = formalArgTypes <$> instFDecl
          argTypes  = mapM typeOfExpr args


castTarget :: TExpr -> String -> Typing TExpr
castTarget trg fname =
  maybe (throwError $ "castTarget error " ++ show (trg, fname)) return =<< castTargetM trg fname

castTargetM :: TExpr -> String -> Typing (Maybe TExpr)
castTargetM trg fname = fmap ($ trg) <$> castTarget' (T.texpr trg) fname id

castTarget'
    :: Typ -> String -> (TExpr -> TExpr) 
    -> Typing (Maybe (TExpr -> TExpr))
castTarget' t fname cast = do
  ci <- lookupClass t
  if isJust (findFeatureInt ci fname) ||
     isJust (findAttrInt ci fname)
    then return (Just cast)
    else (listToMaybe . catMaybes) `fmap` 
         mapM (\ i -> castTarget' i fname (inheritPos (T.Cast i) . cast))
                      (inherit ci)

castAccess :: TExpr -> Typing TExpr
castAccess a = case contents a of
                 T.Access ta aName _ -> castAttr (T.texpr ta) aName a
                 _ -> return a

formalArgTypes :: FeatureI -> [Typ]
formalArgTypes = map declType . featureArgs

argsConform :: [TExpr] -> [Typ] -> Typing ()
argsConform args formArgs
    | length args /= length formArgs = throwError "Differing number of args"
    | otherwise = zipWithM_ conformThrow args formArgs

castAttr :: Typ -> String -> TExpr -> Typing TExpr
castAttr t a e = liftA3 castReturnedValue
                   (declType <$> attrInClassGen a t)
                   (declType <$> attrInClass a t)
                   (pure e)

castResult :: Typ -> String -> TExpr -> Typing TExpr
castResult t f r = liftA3 castReturnedValue
                     (featureResult <$> inClassGen f t)
                     (featureResult <$> inClass f t)
                     (pure r)

castReturnedValue :: Typ -> Typ -> TExpr -> TExpr
castReturnedValue instTyp typ
    | instTyp == typ  = id
    | isBasic instTyp = inheritPos (T.Unbox instTyp)
    | otherwise       = inheritPos (T.Cast instTyp)

castGenericArgsM :: Typ -> String -> [TExpr] -> Typing [TExpr]
castGenericArgsM t fname args =
  zipWith castToStatic args <$> featureArgs <$> fname `inClass` t

castToStatic :: TExpr -> Decl-> TExpr
castToStatic te (Decl _ t@(ClassType _ _))
    | tt == t    = te
    | isBasic tt = inheritPos (T.Box t) te
    | otherwise  = inheritPos (T.Cast t) te
    where
      tt = T.texpr te
castToStatic te _ = te
