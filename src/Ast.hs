module Ast where

type Program = [Decl]

data Decl
  = DVariable Ptrn TypeExpr Expr
  | DFunction Ptrn TypeExpr [Ptrn] [Stmt]
  deriving (Show, Eq)

data Stmt
  = SExpr Expr
  | SReturn Expr
  | SBreak
  | SContinue
  | SPass
  | SVariable Ptrn TypeExpr Expr
  | SFunction Ptrn TypeExpr [Ptrn] [Stmt]
  | SIf [(Expr, [Stmt])] [Stmt]
  | SWhile Expr [Stmt]
  deriving (Show, Eq)

data Expr
  = EInteger Int
  | EVariable String
  | EBoolean Bool
  | ETuple [Expr]
  | ECall Expr Expr
  | EMember Expr String
  | EOpPower Expr Expr
  | EUOpPlus Expr
  | EUOpMinus Expr
  | EOpAdd Expr Expr
  | EOpSubtract Expr Expr
  | EOpMultiply Expr Expr
  | EOpDivide Expr Expr
  | EOpModulo Expr Expr
  | EOpEqual Expr Expr
  | EOpNotEqual Expr Expr
  | EOpLessThan Expr Expr
  | EOpLessEqualThan Expr Expr
  | EOpGreaterThan Expr Expr
  | EOpGreaterEqualThan Expr Expr
  | EOpAssign Expr Expr
  | EOpAssignAdd Expr Expr
  | EOpAssignMultiply Expr Expr
  | ELambda TypeExpr [Ptrn] Expr
  | EOpNegation Expr
  | EOpConjunction Expr Expr
  | EOpAlternative Expr Expr
  | EIf Expr Expr Expr
  | EDereference Expr
  | EAddress Expr
  | ENone
  deriving (Show, Eq)

data Ptrn
  = PNamed String
  | PTuple [Ptrn]
  | PDereference Ptrn
  | PVoid
  deriving (Show, Eq)

data TypeExpr
  = TUnknown
  | TNamed String
  | TInt
  | TBool
  | TVoid
  | TPointer TypeExpr
  | TFunction TypeExpr TypeExpr
  | TTuple [TypeExpr]
  deriving (Show, Eq)
