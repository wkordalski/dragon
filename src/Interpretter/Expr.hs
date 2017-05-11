module Interpretter.Expr where

import Interpretter.Core
import Interpretter.Ptrn

import Debug.Trace
import Control.Monad.Cont
import Control.Monad.Reader

import qualified Ast as A
import qualified Data.Map as M

opName n = "φ_" ++ n

evalLambdaExpr :: Monad m => (M.Map String (Value r m)) -> A.Expr -> IPM r m (Value r m)
evalLambdaExpr sm e =
  callCC $ \k -> do
    let slist = M.toList sm
    slm <- mapM (\(n, v) -> do { a <- allocMemory v; return (n, a) }) slist
    local (\e -> e {
      symbols=(M.fromList slm `M.union` symbols e),
      allSymbols= (snd <$> slm) ++ (allSymbols e),
      returnCont=Just k
      }) $ (evalExpr e)

localLambdaDecl :: Monad m => [A.Ptrn] -> [Value r m] -> A.Expr -> IPMEnvironment r m -> IPM r m (Value r m)
localLambdaDecl ps vs ss env = do
  local (const env) $ do
    sa <- patternsMatchValues ps vs
    evalLambdaExpr sa ss


evalExpr :: Monad m => A.Expr -> IPM r m (Value r m)
evalExpr (A.EInteger n) = return $ VInt n
evalExpr (A.EBoolean b) = return $ VBool b
evalExpr (A.ENone) = return $ VNone
evalExpr (A.EVariable n) = do
  v <- askSymbol n
  return $ VLReference v

evalExpr (A.EAddress e) = do
  (VLReference l) <- evalExpr e
  return $ VPointer l

evalExpr (A.EDereference e) = do
  (VPointer l) <- evalExpr e >>= unreference
  return $ VLReference l

evalExpr (A.ECall fe ae) = do
  (VFunction ac args fun) <- evalExpr fe >>= unreference
  a <- evalExpr ae >>= unreference
  if length args + 1 < ac then
    return $ VFunction ac (a:args) fun
  else
    callCC $ \k -> fun (reverse $ a:args) k

evalExpr (A.ELambda _ ps e) = do
  env <- ask
  let fun args cont = localLambdaDecl ps args e env >>= cont
  let f = VFunction (length ps) [] fun
  return f

evalExpr (A.EOpAdd e1 e2) = evalBinaryOperator (opName "add") e1 e2
evalExpr (A.EOpSubtract e1 e2) = evalBinaryOperator (opName "subtract") e1 e2
evalExpr (A.EOpMultiply e1 e2) = evalBinaryOperator (opName "multiply") e1 e2
evalExpr (A.EOpDivide e1 e2) = evalBinaryOperator (opName "divide") e1 e2
evalExpr (A.EOpLessThan e1 e2) = evalBinaryOperator (opName "less_than") e1 e2
evalExpr (A.EOpLessEqualThan e1 e2) = evalBinaryOperator (opName "less_than_equal") e1 e2
evalExpr (A.EOpGreaterThan e1 e2) = evalBinaryOperator (opName "greater_than") e1 e2
evalExpr (A.EOpGreaterEqualThan e1 e2) = evalBinaryOperator (opName "greater_than_equal") e1 e2
evalExpr (A.EOpEqual e1 e2) = evalBinaryOperator (opName "equal") e1 e2
evalExpr (A.EOpNotEqual e1 e2) = evalBinaryOperator (opName "not_equal") e1 e2

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
