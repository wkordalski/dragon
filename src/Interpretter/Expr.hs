module Interpretter.Expr where

import Interpretter.Core

import qualified Ast as A

unreference :: Value r -> IPM r (Value r)
unreference (VLReference l) = do
  v <- askMemory l
  return v
unreference v = return v

evalExpr :: A.Expr -> IPM r (Value r)
evalExpr (A.EInteger n) = return $ VInt n
evalExpr (A.EVariable n) = do
  v <- askSymbol n
  return $ VLReference v

evalExpr (A.ECall fe ae) = do
  (VFunction ac args fun) <- evalExpr fe >>= unreference
  a <- evalExpr ae >>= unreference
  if length args + 1 < ac then
    return $ VFunction ac (a:args) fun
  else do
    fun (a:args)
