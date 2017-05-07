module Interpretter.Stmt where

import Interpretter.Core
import Interpretter.Expr

import Control.Monad.Cont

import qualified Ast as A

execStmts :: [A.Stmt] -> IPM r ()
execStmts [] = return ()
execStmts (h:t) = execStmt h >> execStmts t


execStmt :: A.Stmt -> IPM r ()
execStmt (A.SReturn e) = callCC $ \_ -> do
  k <- askReturnCont
  v <- evalExpr e >>= unreference
  k v

execStmt (A.SExpr e) = evalExpr e >> return ()

execStmt i@(A.SWhile e s) = callCC $ \k -> do
  (VBool b) <- evalExpr e >>= unreference
  if b then
    --(localContinueBreakCont (execStmt i) k $ (execStmts s >> execStmt i)) >> k
    execStmts s >> execStmt i >> k ()
  else
    k ()
