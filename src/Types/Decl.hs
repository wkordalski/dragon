module Types.Decl (checkDeclsTypes) where

import Types.Core
import Types.Expr
import Types.Ptrn
import Types.Stmt

import qualified Ast as A

import Control.Monad.Except

import qualified Data.Map as M

readDeclType :: A.Decl -> TCM (String, Type)
readDeclType (A.DVariable s t _) = do
  td <- typeFromAst t
  return (s, td)

readDeclType (A.DFunction s t _ _) = do
  td <- typeFromAst t
  return (s, td)

readDeclsTypes :: [A.Decl] -> TCM (M.Map String Type)
readDeclsTypes [] = return M.empty
readDeclsTypes (decl:rest) = do
  (s, t) <- readDeclType decl
  m <- readDeclsTypes rest
  if s `M.member` m then
    throwError $ "Multiple definition of a symbol not supported"
  else
    return $ M.insert s t m

checkDeclsTypes :: [A.Decl] -> TCM ()
checkDeclsTypes ds = do
  stm <- readDeclsTypes ds
  localTypesOf stm $ mapM_ checkDeclTypes ds

checkDeclTypes :: A.Decl -> TCM ()
checkDeclTypes (A.DVariable s t e) = typeFromAst t >>= checkExprType e
checkDeclTypes (A.DFunction s t p stmts) = do
  td <- typeFromAst t
  (tr, mta) <- typeOfNamesInPatterns p td
  localFunction s td mta tr $ checkStmtsTypes stmts
