{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Util.Monad where

import Control.Monad.Reader

import Data.Map

import AST.Class
import AST.Typ

class HasClasEnv r where
    classEnv :: r -> Map String ClasInterface

class (HasClasEnv r, MonadReader r m) => ClassReader r m | m -> r where

askClassEnv :: ClassReader r m => m (Map String ClasInterface)
askClassEnv = classEnv `liftM` ask

lookupClassM :: ClassReader r m => Typ -> m (Maybe ClasInterface)
lookupClassM (ClassType cn _) = Data.Map.lookup cn `liftM` askClassEnv
lookupClassM (Sep _ _ cn)     = lookupClassM (ClassType cn [])
lookupClassM t = error $ "lookupClassM: can't lookup a " ++ show t ++ " type"

lookupClass :: ClassReader r m => Typ -> m ClasInterface
lookupClass t = liftM (maybe (error $ "lookupClas: can't find " ++ show t) id) 
                (lookupClassM t)