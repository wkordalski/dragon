module Interpretter.Program where

import Interpretter.Core
import Interpretter.Decl

import qualified Ast as A

runProgram :: A.Program -> IPM r ()
runProgram p = runDecls p
