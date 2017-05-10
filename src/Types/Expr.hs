module Types.Expr where

import Types.Core
import qualified Ast as A

import Control.Monad.Except
import Data.Map ((!))

opName n = "φ_" ++ n
askTypeOfOp s = askTypeOf $ opName s

-- returns type and boolean which is true if left-value, false if right-value
typeOf' :: A.Expr -> TCM (Type, Bool)
typeOf' (A.EInteger _) = return (TInt, False)
typeOf' (A.EBoolean _) = return (TBool, False)
typeOf' (A.ENone) = return (TVoid, False)
typeOf' (A.EVariable n) = do
  t <- askTypeOf n
  return (t, True)

typeOf' (A.ECall e0 e1) = do
  (t0, _) <- typeOf' e0
  (t1, _) <- typeOf' e1
  mr <- matchType (t1 :-> TPlaceholder 1) t0
  return (mr!1, False)

typeOf' (A.EOpAdd e1 e2) = typeOfBinOp (opName "add") e1 e2
typeOf' (A.EOpSubtract e1 e2) = typeOfBinOp (opName "subtract") e1 e2
typeOf' (A.EOpLessThan e1 e2) = typeOfBinOp (opName "less_than") e1 e2

typeOf' (A.EOpAssign e1 e2) = do
  (t1, l1) <- typeOf' e1
  when (not l1) (throwError $ "LHS of = is not l-value")
  (t2, _) <- typeOf' e2
  mr <- matchType t1 t2
  return (t1, True)

typeOf' (A.EOpAssignAdd e1 e2) = do
  t0 <- askTypeOfOp "add"
  (t1, l1) <- typeOf' e1
  when (not l1) (throwError $ "LHS of += is not l-value")
  (t2, _) <- typeOf' e2
  mr <- matchType (t1 :-> t2 :-> t1) t0
  return (t1, True)


typeOfExpr :: A.Expr -> TCM Type
typeOfExpr expr = do
  (res, islv) <- typeOf' expr
  return res

checkExprType :: A.Expr -> Type -> TCM ()
checkExprType e t = typeOfExpr e >>= matchType t >> return ()


typeOfBinOp n e1 e2 = do
  t0 <- askTypeOf n
  (t1, _) <- typeOf' e1
  (t2, _) <- typeOf' e2
  mr <- matchType (t1 :-> t2 :-> TPlaceholder 1) t0
  return (mr!1, False)
