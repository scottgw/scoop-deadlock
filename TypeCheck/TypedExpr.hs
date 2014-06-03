module TypeCheck.TypedExpr where

import AST.Eiffel (Typ (..), Pos, AbsStmt, PosAbsStmt, ClasBody
                  ,FeatureWithBody, BinOp, UnOp, ClassName, contents)

type TClass = ClasBody TExpr
type TFeature = FeatureWithBody TExpr
type TStmt = PosAbsStmt TExpr
type UnPosTStmt = AbsStmt TExpr

type TExpr = Pos UnPosTExpr

data UnPosTExpr = Call TExpr String [TExpr] Typ
           | Access TExpr String Typ
           | Var String Typ
           | BinOpExpr BinOp TExpr TExpr Typ
           | UnOpExpr UnOp TExpr Typ
           | Attached ClassName TExpr String
           | ResultVar Typ
           | CurrentVar Typ
           | Box Typ TExpr
           | Unbox Typ TExpr
           | Cast Typ TExpr
           | LitChar Char
           | LitString String
           | LitInt Int
           | LitBool Bool
           | LitVoid Typ
           | LitDouble Double deriving (Show, Eq)

texpr :: TExpr -> Typ
texpr = texprTyp . contents

texprTyp :: UnPosTExpr -> Typ
texprTyp (LitInt _)  = IntType
texprTyp (LitBool _) = BoolType
texprTyp (LitDouble _) = DoubleType
texprTyp (LitVoid  t) = t
texprTyp (Var _ t)   = t
texprTyp (Cast t _)  = t
texprTyp (ResultVar t) = t
texprTyp (CurrentVar t) = t
texprTyp (Call _ _ _ t) = t
texprTyp (Access _ _ t) = t
texprTyp (BinOpExpr _ _ _ t) = t
texprTyp (UnOpExpr _ _ t) = t
texprTyp (Box _ te) = texpr te
texprTyp (Unbox t _) = t
texprTyp (LitChar _) = ClassType "CHARACTER" []
texprTyp (Attached _ _ _) = error "texprTyp: attachment test unimplemented"
texprTyp (LitString _) = ClassType "STRING" []