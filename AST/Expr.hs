module AST.Expr where

import AST.Typ
import AST.Position

type Expr = Pos UnPosExpr 

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Or
           | And
           | Implies
           | RelOp ROp Typ
             deriving (Show, Eq)

data ROp = Lte
         | Lt 
         | Eq 
         | TildeEq
         | Neq
         | Gt 
         | Gte
           deriving (Show, Eq)

data UnOp = Not
          | Neg
          | Sqrt
            deriving (Show, Eq)

data UnPosExpr =
    UnqualCall String [Expr]
  | QualCall Expr String [Expr]
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | Attached String Expr String
  | TypedVar String Typ
  | VarOrCall String
  | ResultVar
  | CurrentVar
  | Cast Typ Expr
  | LitString String
  | LitChar Char
  | LitInt Int
  | LitBool Bool
  | LitVoid
  | LitDouble Double deriving (Show, Eq) 


{-
instance Show Expr where
    show (BinOpExpr op l r) = concat ["(", show l, " ",show op, " ", show r, ")"]
    show (LitInt i)   = show i
    show (LitBool b)  = show b
    show (LitDouble d) = show d
    show (Var s)      = s
    show (TypedVar s t) = concat [s, " : ", show t]
    show (QualCall t i args) = concat [show t, ".", i,
                                       "(",concatMap show args,")"]
    show (UnqualCall i args) = concat ["unqual::", i, "(",concatMap show args,")"]
    show (TypedCall trg trgT i args) =
        concat [show trg, "::", show trgT, ".", 
                i, "(", concatMap show args, ")"]
    show (Cast t e) = "cast <" ++ show t ++ "> " ++ show e
-}