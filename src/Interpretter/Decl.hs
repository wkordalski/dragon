module Interpretter.Decl where

import Interpretter.Core
import Interpretter.Expr
import Interpretter.Ptrn
import Interpretter.Stmt

import Control.Monad.Cont
import Control.Monad.Reader

import qualified Ast as A

import qualified Data.Map as M

runDecls :: Monad m => [A.Decl] -> IPM r m ()

runDecls [] = return ()
runDecls ds = do
  dsn <- getNamesOfDecls ds
  localSymbols (M.fromList (map (\e -> (e, VUninitialized)) dsn))
    (initializeFunctions ds >> initializeNonFunctions ds >> return ())

getPatternOfDecl :: A.Decl -> IPM r m A.Ptrn
getPatternOfDecl (A.DVariable p _ _) = return p
getPatternOfDecl (A.DFunction p _ _ _) = return p

getNamesOfDecls :: Monad m => [A.Decl] -> IPM r m [String]
getNamesOfDecls [] = return []
getNamesOfDecls (h:t) = do
  p <- getPatternOfDecl h
  m <- patternMatchValue p VUninitialized
  r <- getNamesOfDecls t
  return $ (fst <$> M.toList m) ++ r

initializeFunctions :: Monad m => [A.Decl] -> IPM r m ()
initializeFunctions ds = mapM_ initializeFunction (filter isFunction ds)

initializeFunction :: Monad m => A.Decl -> IPM r m ()
initializeFunction (A.DFunction p t ps ss) = do
  let (A.PNamed s) = p
  l <- askSymbol s
  env <- ask
  let fun args cont = localFunDecl ps args ss env >>= cont
  let f = VFunction (length ps) [] fun
  setMemory l f


isFunction :: A.Decl -> Bool
isFunction (A.DVariable _ _ _) = False
isFunction (A.DFunction _ _ [] _) = False
isFunction (A.DFunction _ _ _ _) = True

initializeNonFunctions :: Monad m => [A.Decl] -> IPM r m ()
initializeNonFunctions ds =
  mapM_ initializeNonFunction (filter (not . isFunction) ds)

initializeNonFunction :: Monad m => A.Decl -> IPM r m ()
initializeNonFunction (A.DFunction p _ [] ss) = do
  m <- callCC (\k -> (local (\e -> e {returnCont=Just k}) (execStmts ss)) >> return VNone) >>= patternMatchValue p
  mapM_ (\(s, v) -> do { l <- askSymbol s; setMemory l v }) (M.toList m)

initializeNonFunction (A.DVariable p _ e) = do
  v <- evalExpr e
  m <- patternMatchValue p v
  mapM_ (\(s, v) -> do { l <- askSymbol s; setMemory l v }) (M.toList m)

-- zinterpretowanie deklaracji funkcji polega na tym, że tworzymy funkcję.
-- która ustawia odpowiednie kontynuacje w środowisku i sykonuje execStmts na kodzie

-- zinterpretowanie stałej/zmiennej polega na wyliczeniu jej wartości
-- jeśli VUninitialized - to wykonaj monadę
