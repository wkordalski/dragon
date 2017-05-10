module Types.Decl (checkDeclsTypes) where

import Types.Core
import Types.Expr
import Types.Ptrn
import Types.Stmt

import qualified Ast as A

import Control.Monad.Except

import qualified Data.Map as M

readDeclType :: A.Decl -> TCM (M.Map String Type)
readDeclType (A.DVariable p t _) = do
  td <- typeFromAst t
  sm <- typeOfNamesInPattern p td
  return sm

readDeclType (A.DFunction p t _ _) = do
  td <- typeFromAst t
  sm <- typeOfNamesInPattern p td
  return sm

readDeclsTypes :: [A.Decl] -> TCM (M.Map String Type)
readDeclsTypes [] = return M.empty
readDeclsTypes (decl:rest) = do
  sm <- readDeclType decl
  m <- readDeclsTypes rest
  if M.null (sm `M.intersection` m) then
    return $ sm `M.union` m
  else
    throwError $ "Multiple definition of a symbol not supported"

checkDeclsTypes :: [A.Decl] -> TCM ()
checkDeclsTypes ds = do
  stm <- readDeclsTypes ds
  localTypesOf stm $ mapM_ checkDeclTypes ds

checkDeclTypes :: A.Decl -> TCM ()
checkDeclTypes (A.DVariable p t e) = typeFromAst t >>= checkExprType e
checkDeclTypes (A.DFunction p t ps stmts) = do
  td <- typeFromAst t
  (tr, mta) <- typeOfNamesInPatterns ps td
  sm <- typeOfNamesInPattern p td
  localFunction sm mta tr $ checkStmtsTypes stmts
