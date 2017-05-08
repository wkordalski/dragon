module Interpretter.Stmt where

import Interpretter.Core
import Interpretter.Expr

import Control.Monad.Cont
import Control.Monad.Reader

import qualified Ast as A

execStmts :: Monad m => [A.Stmt] -> IPM r m ()
execStmts [] = return ()
execStmts (h:t) = execStmt h >> execStmts t


execStmt :: Monad m => A.Stmt -> IPM r m ()
execStmt (A.SReturn e) = do
  k <- askReturnCont
  v <- evalExpr e >>= unreference
  k v
  return ()

execStmt (A.SExpr e) = evalExpr e >> return ()

execStmt i@(A.SWhile e s) = callCC $ \k -> do
  (VBool b) <- evalExpr e >>= unreference
  if b then
    --(localContinueBreakCont (execStmt i) k $ (execStmts s >> execStmt i)) >> k
    execStmts s >> execStmt i >> k ()
  else
    k ()
