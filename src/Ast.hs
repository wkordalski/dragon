module Ast where

type Program = [Decl]

data Decl
  = DVariable String Type Expr
  | DFunction String Type [Ptrn] [Stmt]
  deriving (Show, Eq)

data Stmt
  = SExpr Expr
  | SReturn Expr
  | SPass
  | SVariable String Type Expr
  | SIf [(Expr, [Stmt])] [Stmt]
  | SWhile Expr [Stmt]
  deriving (Show, Eq)

data Expr
  = EInteger Int
  | EVariable String
  | EOpPlus Expr Expr
  deriving (Show, Eq)

data Ptrn
  = PNamed String
  deriving (Show, Eq)

data Type
  = TUnknown
  | TNamed String
  deriving (Show, Eq)
