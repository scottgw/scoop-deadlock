module TypeCheck.Generic where

import Control.Monad

import TypeCheck.Context

import AST.Eiffel

import Util.Monad

-- for checking declarations `a : ARRAY [INTEGER]'
checkTypeInst :: Typ -> Typing ()
checkTypeInst t@(ClassType _ ts) = do
  clsGens  <- generics `fmap` lookupClass t
  satGensThrow clsGens ts
checkTypeInst _ = return ()

satGensThrow :: [Generic] -> [Typ] -> Typing ()
satGensThrow gs ts = do
  guardThrow (length gs == length ts) "resolveGeneric: diff length"             
  zipWithM_ satGenThrow gs ts
  
satGenThrow :: Generic -> Typ -> Typing ()
satGenThrow g t = do
  sat <- satisfiesGeneric g t
  guardThrow sat "satGenThrow: unsatisfied"

satisfiesGeneric :: Generic -> Typ -> Typing Bool
satisfiesGeneric _g _t = return True

-- during lookup of the class as in `a.f'

resolveIFace :: Typ -> Typing ClasInterface
resolveIFace t@(ClassType _ ts) = updateGenerics ts `fmap` lookupClass t
resolveIFace (Sep _ _ t) = resolveIFace (ClassType t [])
resolveIFace t = error $ "resolveIFace: called on " ++ show t

type GenUpd a = ClassName -> Typ -> a -> a

updateGenerics :: [Typ] -> ClasInterface -> ClasInterface
updateGenerics ts ci =
    let gs = map genericName (generics ci)
        f  = foldl (.) id (zipWith updateGeneric gs ts)
    in f ci

updateGeneric :: GenUpd ClasInterface 
updateGeneric g t ci = 
    ci
    { attributes = map (updateDecl g t) (attributes ci)
    , features   = map (updateFeatDecl g t) (features ci)
    }

updateFeatDecl :: GenUpd FeatureI
updateFeatDecl g t fd = 
    fd 
    { featureArgs = map (updateDecl g t) (featureArgs fd)
    , featureResult = updateTyp g t (featureResult fd)
    }

updateDecl :: GenUpd Decl
updateDecl g t (Decl n t') = Decl n (updateTyp g t t')

updateTyp :: GenUpd Typ
updateTyp g t t'@(ClassType c' _)
    | g == c'   = t --ClassType  (map (updateTyp g t) gs)
    | otherwise = t'
updateTyp _ _ t' = t'