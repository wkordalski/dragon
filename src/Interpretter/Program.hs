module Interpretter.Program where

import Interpretter.Core
import Interpretter.Decl

import qualified Ast as A

runProgram :: Monad m => A.Program -> IPM r m ()
runProgram p = runDecls p
