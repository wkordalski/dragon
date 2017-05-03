module Types.Stmt where

import Types.Core
import Types.Expr
import Types.Ptrn

import Control.Monad.Except

import qualified Ast as A


checkStmtsTypes :: [A.Stmt] -> TCM ()
checkStmtsTypes [] = do
  b <- askAutoReturn
  when b $ askReturnType >>= matchType TVoid >> return ()

checkStmtsTypes (A.SExpr e : rest) = typeOfExpr e >> checkStmtsTypes rest
checkStmtsTypes (A.SReturn e : []) = askReturnType >>= checkExprType e
checkStmtsTypes (A.SReturn e : _) = throwError $ "Unreachable code!"

checkStmtsTypes (A.SPass : rest) = checkStmtsTypes rest
checkStmtsTypes (A.SVariable s t e : rest) = do
  te <- typeOfExpr e
  td <- typeFromAst t
  mr <- matchType td te
  localTypeOf s te $ checkStmtsTypes rest

checkStmtsTypes (A.SFunction s t p stmts : rest) = do
  td <- typeFromAst t
  (tr, mta) <- typeOfNamesInPatterns p td
  localFunction s td mta tr $ checkStmtsTypes stmts
  localTypeOf s td $ checkStmtsTypes rest

checkStmtsTypes (A.SIf conds nocond : rest) =
  mapM_ (\(e,s) ->
    localNoAutoReturn (checkExprType e TBool >> checkStmtsTypes s)
    ) conds
  >> (localNoAutoReturn $ checkStmtsTypes nocond)
  >> checkStmtsTypes rest

checkStmtsTypes (A.SWhile cond stmts : rest) =
  checkExprType cond TBool
  >> (localNoAutoReturn $ checkStmtsTypes stmts)
  >> checkStmtsTypes rest
