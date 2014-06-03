{-# LANGUAGE ViewPatterns #-}

module Deadlock.Instantiate 
    (
      instantiateSep
    , addImplLocks
    , checkScope
    , instFeat
    , declProcs
    , exprProcs
    , typProc
    ) where

import Data.Maybe

import Control.Monad.Error

import AST.Eiffel

import Deadlock.Monad
import Deadlock.Error

import qualified TypeCheck.TypedExpr as T
import TypeCheck.TypedExpr (TExpr, TClass)

checkScope :: TClass -> DeadFeature ()
checkScope = maybe (return ()) 
             (mapM_ throwPosError) . checkScope' . clasInterface

checkScope' :: ClasInterface -> Maybe [DeadError]
checkScope' ci = 
    let ps = Dot : procGeneric ci
        fs = features ci
    in sequence (map (checkScopeFeat ps) fs)

checkScopeFeat :: [Proc] -> FeatureI -> Maybe DeadError
checkScopeFeat ps f = 
    let argsProcs = concat $ catMaybes $ map declProcs (featureArgs f)
        defProcs  = ps ++ argsProcs
        res       = maybe [] id (getProcs $ featureResult f)
        allProcs  = featureEnsLk f ++ res
        undefd    = filter (not . flip elem defProcs) allProcs
    in 
      case undefd of
        []    -> Nothing
        (u:_) -> Just (ProcNotInScope u)

instantiateSep :: Typ -> ClasInterface -> Dead ClasInterface
instantiateSep (Sep p ps _) ci
  | length ps /= length gs = throwPosError InstProcsMismatch
  | otherwise = return (instSeps ci (zip (currProc ci:gs) (p:ps)))
  where gs = procGeneric ci
instantiateSep t _ = error $ "instantiateSep: can't instantiate " ++ show t

instSeps :: ClasInterface -> [(Proc,Proc)] -> ClasInterface
instSeps = foldl instSep

instSep :: ClasInterface -> (Proc,Proc) -> ClasInterface
instSep ci (key,val) = 
    let attrs = attributes  ci
        feats = features    ci
        procs = procGeneric ci
    in
      ci { currProc    = updGen key val (currProc ci)
         , procGeneric = map (updGen key val) procs
         , attributes  = map (instDecl key val) attrs
         , features    = map (instFeat key val) feats
         }

instDecl :: Proc -> Proc -> Decl -> Decl
instDecl key val (Decl n t) = Decl n (instTyp key val t) 

instTyp :: Proc -> Proc -> Typ -> Typ
instTyp key val (Sep p ps t) = let f = updGen key val in Sep (f p) (map f ps) t
instTyp _   _   t          = t

instFeat :: Proc -> Proc -> FeatureI -> FeatureI
instFeat key val f = 
    let args = featureArgs f
        res  = featureResult f
        reqs = featureReqLk f
        lcks = featureEnsLk f
    in
      f { featureArgs   = map (instDecl key val) args
        , featureResult = instTyp key val res
        , featureReqLk  = map (updProcExpr key val) reqs
        , featureEnsLk  = map (updGen key val) lcks
        }

updProcExpr :: Proc -> Proc -> ProcExpr -> ProcExpr
updProcExpr key val (LessThan a b) = 
    LessThan (updGen key val a)
             (updGen key val b)

updGen :: Proc -> Proc -> Proc -> Proc
updGen key val rep 
    | key == rep = val
    | otherwise  = rep

declProcs :: Decl -> Maybe [Proc]
declProcs = getProcs . declType

exprProcs :: TExpr -> Maybe [Proc]
exprProcs = getType >=> getProcs

getType :: TExpr -> Maybe Typ
getType (contents -> T.Call _ _ _ t) = Just t
getType (contents -> T.Var _ t)      = Just t
getType (contents -> _) = Nothing
                      
getProcs :: Typ -> Maybe [Proc]            
getProcs (Sep p ps _) = Just (p:ps)
getProcs _            = Nothing

typProc :: Typ -> Proc
typProc (Sep p _ _) = p
typProc _            = Dot

addImplLocks :: AbsClas body exp -> AbsClas body exp
addImplLocks c = c { features = map implicitLocks (features c)}

implicitLocks :: AbsFeature body exp -> AbsFeature body exp
implicitLocks feat = feat {featureReqLk = featureReqLk feat ++ reqs}
    where reqs = map (flip LessThan Dot . typProc . declType) (featureArgs feat)