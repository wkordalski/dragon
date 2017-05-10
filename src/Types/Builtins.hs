module Types.Builtins where

import Types.Core

import qualified Data.Map as M

opName n = "φ_" ++ n


withBuiltins :: TCM a -> TCM a
withBuiltins = do
  localTypesOf (
    M.fromList [
      (opName "add", TInt :-> TInt :-> TInt),
      (opName "subtract", TInt :-> TInt :-> TInt),
      (opName "multiply", TInt :-> TInt :-> TInt),
      (opName "divide", TInt :-> TInt :-> TInt),
      (opName "less_than", TInt :-> TInt :-> TBool),
      ("print", TInt :-> TVoid)
    ]
    )
