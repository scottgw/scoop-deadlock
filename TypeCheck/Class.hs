module TypeCheck.Class (typeCheckClass, typeCheckClassM) where

import Control.Monad.Error
import Control.Monad.Reader

import AST.Eiffel

import qualified TypeCheck.TypedExpr as T
import TypeCheck.TypedExpr (TStmt, TFeature, TClass)

import TypeCheck.BasicTypes
import TypeCheck.Context
import TypeCheck.Expr

featureEnv :: Feature -> TypeContext -> TypeContext
featureEnv f = 
    addDecls (featureArgs f ++ featureLocal (featureImpl f)) . setResult f
  
typeCheckClassM :: [ClasInterface] -> Clas -> Either String TClass
typeCheckClassM cs c = idErrorRead (typeCheckClass c) (mkCtx c cs)

typeCheckClass :: Clas -> Typing TClass
typeCheckClass c = do
    fs   <- mapM typeCheckFeature (features c)
    invs <- mapM clause (invnts c)
    return $ c {features = fs, invnts = invs}

--    reqs <- mapM typeOfExpr (featureReq c)
--    enss <- mapM typeOfExpr (featureEns c)
-- featureReq = reqs, featureEns = enss}

typeCheckFeature :: Feature -> Typing TFeature
typeCheckFeature f = 
    local (featureEnv f) 
              (do
                pre  <- mapM clause (featureReq f)
                post <- mapM clause (featureEns f)
                body <- typeCheckFeature' f
                return $ updFeat f pre body post
              )

updFeat :: FeatureWithBody exp -> [Clause exp2] 
        -> PosAbsStmt exp2 -> [Clause exp2] -> FeatureWithBody exp2
updFeat f pre body post = 
    f { featureImpl = updFeatBody (featureImpl f) body
      , featureReq = pre
      , featureEns = post}

typeCheckFeature' :: Feature -> Typing TStmt
typeCheckFeature' = typeCheckStmt . featureBody . featureImpl

typeCheckStmt :: Stmt -> Typing TStmt
typeCheckStmt s = setPosition (position s) (typeCheckUPStmt (contents s))

typeCheckUPStmt :: UnPosStmt -> Typing TStmt
typeCheckUPStmt (CallStmt e) = do
  e' <- typeOfExpr e
  tagPos (CallStmt e')
typeCheckUPStmt (Assign s e) = do
  s' <- typeOfExpr s
  e' <- typeOfExpr e
  e'' <- conformThrow e' (T.texpr s')
  return $ inheritPos (Assign s') e''
--  t  <- conformThrow e' tVar
--  case contents e' of
--    T.LitVoid _ -> tagPos (T.LitVoid t) >>= tagPos . Assign s
--    _ -> tagPos (Assign s e')
typeCheckUPStmt (If b s1 s2) = do
  b' <- typeOfExpr b
  guardThrow (isBool $ T.texpr b') "typeCheckStmt: If doesn't get Bool"
  s1' <- typeCheckStmt s1
  s2' <- typeCheckStmt s2
  tagPos (If b' s1' s2')
typeCheckUPStmt (Loop setup cond body) = do
  setup' <- typeCheckStmt setup
  cond' <- typeOfExpr cond
  case T.texpr cond' of
    BoolType -> return ()
    _        -> throwError "loop condition should be of type boolean"
  body' <- typeCheckStmt body
  tagPos (Loop setup' cond' body')
typeCheckUPStmt (Block ss) = Block `fmap` mapM typeCheckStmt ss >>= tagPos
typeCheckUPStmt (Print e)  = do
  e' <- typeOfExpr e
  guardThrow (isInt $ T.texpr e') "typeCheckUPStmt: Print not int"
  tagPos (Print e')
typeCheckUPStmt (PrintD e)  = do
  e' <- typeOfExpr e
  guardThrow (isDouble $ T.texpr e') "typeCheckUPStmt: Print not double"
  tagPos (PrintD e')
typeCheckUPStmt (Create vr fName args) = do
  call <- tagPos (QualCall vr fName args) >>= typeOfExpr
  let call' = case contents call of
                T.Cast _ c -> c
                T.Call _ _ _ _ -> call
                _ -> error "typeCheckUPStmt: create only on casts or calls"
  let T.Call trg _ tArgs res = contents call'
  let ClassType _ _ = T.texpr trg
  guardThrow (res == NoType) 
                 "There must be no return type for create"
  tagPos (Create trg fName tArgs)
typeCheckUPStmt (DefCreate v) = let VarOrCall _ = contents v in
                                typeCheckUPStmt (Create v  "make" [])
typeCheckUPStmt BuiltIn = tagPos BuiltIn
typeCheckUPStmt s = error $ "typeCheckUPStmt: not implemented " ++ show s