module Interpretter.Expr where

import Interpretter.Core

import Debug.Trace
import Control.Monad.Cont
import Control.Monad.Reader

import qualified Ast as A

unreference :: Monad m => Value r m -> IPM r m (Value r m)
unreference (VLReference l) = do
  v <- askMemory l
  return v
unreference v = return v

evalExpr :: Monad m => A.Expr -> IPM r m (Value r m)
evalExpr (A.EInteger n) = return $ VInt n
evalExpr (A.ENone) = return $ VNone
evalExpr (A.EVariable n) = do
  v <- askSymbol n
  return $ VLReference v

evalExpr (A.ECall fe ae) = do
  (VFunction ac args fun) <- evalExpr fe >>= unreference
  a <- evalExpr ae >>= unreference
  if length args + 1 < ac then
    return $ VFunction ac (a:args) fun
  else do
    v <- callCC $ \k -> fun (a:args) k
    return v
