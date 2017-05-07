module Interpretter.Decl where

import Interpretter.Core
import Interpretter.Ptrn
import Interpretter.Stmt

import Control.Monad.Cont
import Control.Monad.Reader

import qualified Ast as A

import qualified Data.Map as M

runDecls :: [A.Decl] -> IPM r ()

runDecls [] = return ()
runDecls ds = do
  dsn <- getNamesOfDecls ds
  localSymbols (M.fromList (map (\e -> (e, VUninitialized)) dsn))
    (initializeFunctions ds >> initializeNonFunctions ds >> return ())

getPatternOfDecl :: A.Decl -> IPM r A.Ptrn
getPatternOfDecl (A.DVariable p _ _) = return p
getPatternOfDecl (A.DFunction p _ _ _) = return p

getNamesOfDecls :: [A.Decl] -> IPM r [String]
getNamesOfDecls [] = return []
getNamesOfDecls (h:t) = do
  p <- getPatternOfDecl h
  m <- patternMatchValue p VUninitialized
  r <- getNamesOfDecls t
  return $ (fst <$> M.toList m) ++ r

initializeFunctions :: [A.Decl] -> IPM r ()
initializeFunctions ds = mapM_ initializeFunction (filter isFunction ds)

initializeFunction :: A.Decl -> IPM r ()
initializeFunction (A.DFunction p t ps ss) = do
  let (A.PNamed s) = p
  l <- askSymbol s
  let fun args = callCC $ \k -> localFunDecl ps args k $ execStmts ss >> k VNone
  let f = VFunction (length ps) [] fun
  setMemory l f

localFunDecl :: [A.Ptrn] -> [Value r] -> (Value r -> IPM r (Value r)) -> IPM r (Value r) -> IPM r (Value r)
localFunDecl ps vs k m = do
  sa <- patternsMatchValues ps vs
  localSymbols sa $ local (\e -> e {returnCont=Just k}) m

isFunction :: A.Decl -> Bool
isFunction (A.DVariable _ _ _) = False
isFunction (A.DFunction _ _ [] _) = False
isFunction (A.DFunction _ _ _ _) = True

initializeNonFunctions :: [A.Decl] -> IPM r ()
initializeNonFunctions ds = return ()
-- zinterpretowanie deklaracji funkcji polega na tym, że tworzymy funkcję.
-- która ustawia odpowiednie kontynuacje w środowisku i sykonuje execStmts na kodzie

-- zinterpretowanie stałej/zmiennej polega na wyliczeniu jej wartości
-- jeśli VUninitialized - to wykonaj monadę
