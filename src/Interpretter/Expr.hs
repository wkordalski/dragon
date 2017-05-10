module Interpretter.Expr where

import Interpretter.Core

import Debug.Trace
import Control.Monad.Cont
import Control.Monad.Reader

import qualified Ast as A

opName n = "φ_" ++ n

unreference :: Monad m => Value r m -> IPM r m (Value r m)
unreference (VLReference l) = do
  v <- askMemory l
  return v
unreference v = return v

evalExpr :: Monad m => A.Expr -> IPM r m (Value r m)
evalExpr (A.EInteger n) = return $ VInt n
evalExpr (A.EBoolean b) = return $ VBool b
evalExpr (A.ENone) = return $ VNone
evalExpr (A.EVariable n) = do
  v <- askSymbol n
  return $ VLReference v

evalExpr (A.ECall fe ae) = do
  (VFunction ac args fun) <- evalExpr fe >>= unreference
  a <- evalExpr ae >>= unreference
  if length args + 1 < ac then
    return $ VFunction ac (a:args) fun
  else
    callCC $ \k -> fun (reverse $ a:args) k

evalExpr (A.EOpAdd e1 e2) = evalBinaryOperator (opName "add") e1 e2
evalExpr (A.EOpSubtract e1 e2) = evalBinaryOperator (opName "subtract") e1 e2
evalExpr (A.EOpLessThan e1 e2) = evalBinaryOperator (opName "less_than") e1 e2

evalExpr (A.EOpAssign e1 e2) = do
  (VLReference l1) <- evalExpr e1
  a2 <- evalExpr e2 >>= unreference
  setMemory l1 a2
  return $ VLReference l1


evalBinaryOperator n e1 e2 = do
  (VFunction ac args fun) <- askSymbol n >>= askMemory >>= unreference
  a1 <- evalExpr e1 >>= unreference
  a2 <- evalExpr e2 >>= unreference
  if length args + 2 < ac then
    return $ VFunction ac (a2:a1:args) fun
  else
    callCC $ \k -> fun (reverse $ a2:a1:args) k
