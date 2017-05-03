module Types.Program where

import Types.Core
import Types.Decl

import qualified Ast as A

checkProgramTypes :: A.Program -> TCM ()
checkProgramTypes p = checkDeclsTypes p
