{-# LANGUAGE KindSignatures #-}

module AST.Feature where

import Data.Map (Map)

import AST.Clause
import AST.Decl
import AST.Expr
import AST.Stmt
import AST.Typ


type FeatureI = AbsFeature EmptyBody Expr
type FeatureWithBody exp = AbsFeature FeatureBody exp
type Feature = FeatureWithBody Expr

data EmptyBody exp = EmptyBody deriving Show

data AbsFeature (body :: * -> *) exp = 
    AbsFeature 
    { 
      featureFroz   :: Bool,
      featureName   :: String,
      featureArgs   :: [Decl],
      featureResult :: Typ,
      featureProcs  :: [Proc],
      featureReq    :: [Clause exp],
      featureReqLk  :: [ProcExpr],

      featureImpl   :: body exp,

      featureEns    :: [Clause exp],
      featureEnsLk  :: [Proc]
    } deriving Show

data FeatureBody exp =
    FeatureBody 
    {
      featureLocal :: [Decl],
      featureLocalProcs :: [ProcDecl],
      featureBody  :: PosAbsStmt exp
    } deriving Show

makeFeatureI :: AbsFeature body exp -> FeatureI
makeFeatureI f = f {featureReq = [], featureEns = [], featureImpl = EmptyBody}

argMap :: FeatureWithBody a -> Map String Typ
argMap = declsToMap . featureArgs

localMap :: FeatureWithBody a -> Map String Typ
localMap = declsToMap . featureLocal . featureImpl

updFeatBody :: FeatureBody a -> PosAbsStmt b -> FeatureBody b
updFeatBody impl body = impl {featureBody = body}

{-
instance Show FeatureDecl where
    show (FeatureDecl name fr procEns args res pGens) =
        let 
            resStr = case res of
                       NoType -> ""
                       t      -> ":" ++ show t
            frz = if fr then "frozen" else ""
        in
          frz ++ 
          name ++ "(" ++ concat (intersperse "," (map show args)) ++ ")" ++ 
                  resStr ++ show pGens
-}