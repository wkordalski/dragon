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
  f <- evalExpr fe >>= unreference
  a <- evalExpr ae >>= unreference
  callFunction f [a]

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

evalExpr (A.EOpAssignAdd e1 e2) = evalBinaryAssignOperator (opName "add") e1 e2
evalExpr (A.EOpAssignSubtract e1 e2) = evalBinaryAssignOperator (opName "subtract") e1 e2
evalExpr (A.EOpAssignMultiply e1 e2) = evalBinaryAssignOperator (opName "multiply") e1 e2
evalExpr (A.EOpAssignDivide e1 e2) = evalBinaryAssignOperator (opName "divide") e1 e2


callFunction (VFunction ac applied fun) args =
  if length applied + length args < ac then
    return $ VFunction ac (args ++ applied) fun
  else
    callCC $ \k -> fun (reverse $ args ++ applied) k

evalBinaryOperator n e1 e2 = do
  f <- askSymbol n >>= askMemory >>= unreference
  a1 <- evalExpr e1 >>= unreference
  a2 <- evalExpr e2 >>= unreference
  callFunction f [a2, a1]

evalBinaryAssignOperator n e1 e2 = do
  f <- askSymbol n >>= askMemory >>= unreference
  r1@(VLReference l1) <- evalExpr e1
  a1 <- unreference r1
  a2 <- evalExpr e2 >>= unreference
  v <- callFunction f [a2, a1]
  setMemory l1 v
  return r1
