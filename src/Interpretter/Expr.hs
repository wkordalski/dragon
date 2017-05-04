module Interpretter.Expr where

import Interpretter.Core

import qualified Ast as A

unreference :: Value -> IPM r Value
unreference (VLReference l) = do
  v <- askMemory l
  return v

evalExpr :: A.Expr -> IPM r Value
evalExpr (A.EInteger n) = return $ VInt n
evalExpr (A.EVariable n) = do
  v <- askSymbol n
  return $ VLReference v
