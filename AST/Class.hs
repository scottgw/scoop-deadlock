{-# LANGUAGE KindSignatures #-}

module AST.Class where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (listToMaybe)

import AST.Decl
import AST.Clause
import AST.Expr
import AST.Feature
import AST.Typ

type Clas = ClasBody Expr
type ClasBody exp = AbsClas FeatureBody exp
type ClasInterface = AbsClas EmptyBody Expr
type ClasI exp = AbsClas FeatureBody exp

data AbsClas (body :: * -> *) exp =
    AbsClas
    {
      className  :: ClassName,
      redefine   :: Redefine,
      currProc   :: Proc,
      procGeneric :: [Proc],
      procExpr   :: [ProcDecl],
      generics   :: [Generic],
      inherit    :: [Typ],
      creates    :: [String],
      attributes :: [Decl],
      features   :: [AbsFeature body exp],
      invnts     :: [Clause exp]
    } deriving Show

newtype Redefine = Redefine {unRedefine :: [String]} deriving Show

data Generic = Generic ClassName deriving Show 

mapFeatures :: (AbsFeature body exp -> AbsFeature body exp) 
            -> AbsClas body exp -> AbsClas body exp
mapFeatures f c = c {features = map f (features c)}

clasInterface :: AbsClas body exp -> ClasInterface
clasInterface c = c {features = map makeFeatureI (features c), invnts = []}

clasMap :: [AbsClas body exp] -> Map ClassName (AbsClas body exp)
clasMap = Map.fromList . map (\ c -> (className c, c))

attrMap :: AbsClas body exp -> Map String Typ
attrMap = declsToMap . attributes

findFeature :: Clas -> String -> Maybe Feature
findFeature c fName =
    let fs = features c
        ffs = filter ( (== fName) . featureName) fs
    in listToMaybe ffs

findFeatureInt :: ClasInterface -> String -> Maybe FeatureI
findFeatureInt c fName =
    let fs = features c
        ffs = filter ( (== fName) . featureName) fs
    in listToMaybe ffs

findAttrInt :: ClasInterface -> String -> Maybe Decl
findAttrInt c attrName = 
    let as = attributes c
        as' = filter ( ( == attrName) . declName) as
    in listToMaybe as'

updFeatures :: AbsClas body exp -> [AbsFeature body exp] -> AbsClas body exp
updFeatures c fs = c {features = fs}

fullName :: AbsClas body exp -> FeatureI -> String
fullName c f = fullNameStr (className c) (featureName f)

fullNameStr :: String -> String -> String
fullNameStr = (++)

genericName :: Generic -> String
genericName (Generic cn) = cn

genericStubs :: AbsClas body exp -> [AbsClas body exp]
genericStubs = map makeGenericStub . generics

-- for the G,H in something like `class A [G,H]'
makeGenericStub :: Generic -> AbsClas body exp
makeGenericStub (Generic g) = AbsClas 
                  { className  = g
                  , redefine   = Redefine []
                  , currProc   = Dot
                  , procGeneric = []
                  , procExpr   = []
                  , generics   = []
                  , inherit    = []
                  , creates    = []
                  , attributes = []
                  , features   = []
                  , invnts     = []
                  }