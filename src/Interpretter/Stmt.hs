module Interpretter.Stmt where

import Interpretter.Core
import Interpretter.Expr

import Control.Monad.Cont

import qualified Ast as A

execStmts :: [A.Stmt] -> IPM r IPMCont
execStmts [] = return CNormal
execStmts (h:t) = execStmt h >>= do_rest where
  do_rest CNormal = execStmts t
  do_rest r = return r


execStmt :: A.Stmt -> IPM r IPMCont
execStmt (A.SReturn e) =
  evalExpr e >>= unreference >>= (\v -> return $ CReturn v)

execStmt (A.SExpr e) = evalExpr e >> return CNormal

execStmt (A.SWhile e s) = do
  (VBool b) <- evalExpr e >>= unreference
  if b then
    execStmts s
  else
    return $ CNormal
