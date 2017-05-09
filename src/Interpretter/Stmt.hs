module Interpretter.Stmt where

import Interpretter.Core
import Interpretter.Expr

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader

import qualified Ast as A

execStmts :: Monad m => [A.Stmt] -> IPM r m ()
execStmts [] = return ()
execStmts (A.SPass : t) = execStmts t

execStmts (A.SReturn e : t) = do
  k <- askReturnCont
  v <- evalExpr e >>= unreference
  k v
  return ()

execStmts (A.SExpr e : t) = evalExpr e >> execStmts t

execStmts i@(A.SWhile e s : t) = do
  (VBool b) <- evalExpr e >>= unreference
  if b then
    execStmts s >> execStmts i
  else
    execStmts t

execStmts (A.SIf [] es : t) = execStmts es >> execStmts t
execStmts (A.SIf ((c,s):cr) es : t) = do
  (VBool b) <- evalExpr c >>= unreference
  if b then
    execStmts s >> execStmts t
  else
    execStmts (A.SIf cr es : t)
