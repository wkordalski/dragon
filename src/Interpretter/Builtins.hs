module Interpretter.Builtins where

import Interpretter.Core

import Control.Monad.Writer

import qualified Data.Map as M

withBuiltins :: IPM r a -> IPM r a
withBuiltins = do
  localSymbols (
    M.fromList [
      ("print", VFunction 1 [] printFun)
    ]
    )

printFun :: [Value r] -> IPM r (Value r)
printFun [VInt n] = do
  tell $ show n ++ "\n"
  return VNone
