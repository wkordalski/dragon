module Types.Expr where

import Types.Core
import Types.Match
import Types.Ptrn
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

typeOf' (A.ETuple l) = do
  tl <- mapM (\e -> do {(t, _) <- typeOf' e; return t}) l
  return $ (TTuple tl, False)

typeOf' (A.EAddress e) = do
  (t, l) <- typeOf' e
  when (not l) (throwError $ "Address operator argument is not l-value")
  return (TPointer t, False)

typeOf' (A.EDereference e) = do
  (t, _) <- typeOf' e
  mr <- matchType (TPointer $ TPlaceholder 1) t
  return (mr!1, True)

typeOf' (A.ECall e0 e1) = do
  (t0, _) <- typeOf' e0
  (t1, _) <- typeOf' e1
  mr <- matchType (t1 :-> TPlaceholder 1) t0
  return (mr!1, False)

typeOf' (A.ELambda t ps e) = do
  td <- typeFromAst t
  (tr, sm) <- typeOfNamesInPatterns ps td
  (te, _) <- localTypesOf sm $ typeOf' e
  mr <- matchType tr te
  return (td, False)

typeOf' (A.EOpAdd e1 e2) = typeOfBinOp (opName "add") e1 e2
typeOf' (A.EOpSubtract e1 e2) = typeOfBinOp (opName "subtract") e1 e2
typeOf' (A.EOpMultiply e1 e2) = typeOfBinOp (opName "multiply") e1 e2
typeOf' (A.EOpDivide e1 e2) = typeOfBinOp (opName "divide") e1 e2
typeOf' (A.EOpLessThan e1 e2) = typeOfBinOp (opName "less_than") e1 e2
typeOf' (A.EOpLessEqualThan e1 e2) = typeOfBinOp (opName "less_than_equal") e1 e2
typeOf' (A.EOpGreaterThan e1 e2) = typeOfBinOp (opName "greater_than") e1 e2
typeOf' (A.EOpGreaterEqualThan e1 e2) = typeOfBinOp (opName "greater_than_equal") e1 e2

typeOf' (A.EOpEqual e1 e2) = do
  (t1, _) <- typeOf' e1
  (t2, _) <- typeOf' e2
  when (t1 /= t2) $ throwError "Calling equal operator on different-types values"
  when (t1 /= TInt && t1 /= TBool) $ throwError "Calling equal on non-basic types - that is int or bool"
  return (TBool, False)

typeOf' (A.EOpNotEqual e1 e2) = do
  (t1, _) <- typeOf' e1
  (t2, _) <- typeOf' e2
  when (t1 /= t2) $ throwError "Calling not-equal operator on different-types values"
  when (t1 /= TInt && t1 /= TBool) $ throwError "Calling not-equal on non-basic types - that is int or bool"
  return (TBool, False)

typeOf' (A.EOpAssign e1 e2) = do
  (t1, l1) <- typeOf' e1
  when (not l1) (throwError $ "LHS of = is not l-value")
  (t2, _) <- typeOf' e2
  mr <- matchType t1 t2
  return (t1, True)

typeOf' (A.EOpAssignAdd e1 e2) = typeOfBinAssignOp "add" e1 e2
typeOf' (A.EOpAssignSubtract e1 e2) = typeOfBinAssignOp "subtract" e1 e2
typeOf' (A.EOpAssignMultiply e1 e2) = typeOfBinAssignOp "multiply" e1 e2
typeOf' (A.EOpAssignDivide e1 e2) = typeOfBinAssignOp "divide" e1 e2


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

typeOfBinAssignOp n e1 e2 = do
  t0 <- askTypeOfOp n
  (t1, l1) <- typeOf' e1
  when (not l1) (throwError $ "LHS of "++n++" operator is not l-value")
  (t2, _) <- typeOf' e2
  mr <- matchType (t1 :-> t2 :-> t1) t0
  return (t1, True)
