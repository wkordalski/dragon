module Types.Builtins where

import Types.Core

import qualified Data.Map as M

opName n = "φ_" ++ n


withBuiltins :: TCM a -> TCM a
withBuiltins = do
  localTypesOf (
    M.fromList [
      (opName "add", TInt :-> TInt :-> TInt),
      ("print", TInt :-> TVoid)
    ]
    )
