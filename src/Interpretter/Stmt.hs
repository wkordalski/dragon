module Interpretter.Stmt where

import Interpretter.Core
import Interpretter.Expr
import Interpretter.Ptrn

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader

import qualified Ast as A
import qualified Data.Map as M

execFunctionStmts :: Monad m => (M.Map String (Value r m)) -> [A.Stmt] -> IPM r m (Value r m)
execFunctionStmts sm ss =
  callCC $ \k -> do
    let slist = M.toList sm
    slm <- mapM (\(n, v) -> do { a <- allocMemory v; return (n, a) }) slist
    local (\e -> e {
      symbols=(M.fromList slm `M.union` symbols e),
      allSymbols= (snd <$> slm) ++ (allSymbols e),
      returnCont=Just k
      }) $ (execStmts ss >> return VNone)

localFunDecl :: Monad m => [A.Ptrn] -> [Value r m] -> [A.Stmt] -> IPMEnvironment r m -> IPM r m (Value r m)
localFunDecl ps vs ss env = do
  local (const env) $ do
    sa <- patternsMatchValues ps vs
    execFunctionStmts sa ss


execStmts :: Monad m => [A.Stmt] -> IPM r m ()
execStmts [] = return ()
execStmts (A.SPass : t) = execStmts t

execStmts (A.SReturn e : t) = do
  k <- askReturnCont
  v <- evalExpr e >>= unreference
  k v
  return ()

execStmts (A.SExpr e : t) = evalExpr e >> execStmts t

execStmts (A.SVariable p _ e : t) = do
  sm <- evalExpr e >>= patternMatchValue p
  localSymbols sm $ execStmts t
  return ()

execStmts (A.SFunction p _ [] ss : t) = do
  sm <- execFunctionStmts M.empty ss >>= patternMatchValue p
  localSymbols sm $ execStmts t

execStmts (A.SFunction p _ ps ss : t) = do
  m <- patternMatchValue p VUninitialized
  localSymbols m $ do
    let (A.PNamed s) = p
    l <- askSymbol s
    r <- ask
    let fun args cont = localFunDecl ps args ss r >>= cont
    let f = VFunction (length ps) [] fun
    setMemory l f
    execStmts t

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
