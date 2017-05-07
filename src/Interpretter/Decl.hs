module Interpretter.Decl where

import Interpretter.Core
import Interpretter.Ptrn

import qualified Ast as A

import qualified Data.Map as M

runDecls :: [A.Decl] -> IPM r ()

runDecls [] = return ()
-- allocateSymbols, then run the rest with changed env
-- the rest is:
-- load all function-like decls
-- eval all constants/variables

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

allocateSymbols :: [A.Decl] -> IPM r (M.Map String Loc)
allocateSymbols ds = do
  ss <- getNamesOfDecls ds
  ms <- mapM (\n -> do { a <- allocMemory (VUninitialized); return (n, a) }) ss
  return $ M.fromList ms


-- zinterpretuj funkcje
-- zinterpretuj stałe/zmienne od góry do dołu

-- zinterpretowanie deklaracji funkcji polega na tym, że tworzymy funkcję.
-- która ustawia odpowiednie kontynuacje w środowisku i sykonuje execStmts na kodzie

-- zinterpretowanie stałej/zmiennej polega na wyliczeniu jej wartości
-- jeśli VUninitialized - to wykonaj monadę
