module Types.Builtins where

import Types.Core

import qualified Data.Map as M

opName n = "φ_" ++ n


withBuiltins :: TCM a -> TCM a
withBuiltins m = do
  let builtins = M.fromList [(opName "add", TInt :-> TInt :-> TInt)]
  localTypesOf builtins m
