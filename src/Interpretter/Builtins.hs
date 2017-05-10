{-# LANGUAGE BangPatterns #-}

module Interpretter.Builtins where

import Interpretter.Core

import Debug.Trace

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as M

opName n = "φ_" ++ n

arithmetic = [
  (opName "add", VFunction 2 [] $ \[VInt a, VInt b] k -> k (VInt $ a+b)),
  (opName "subtract", VFunction 2 [] $ \[VInt a, VInt b] k -> k (VInt $ a-b)),
  (opName "multiply", VFunction 2 [] $ \[VInt a, VInt b] k -> k (VInt $ a*b)),
  (opName "divide", VFunction 2 [] $ \[VInt a, VInt b] k ->
    if b == 0 then throwError "Division by 0 error!" else k (VInt $ a `quot` b)
    ),
  (opName "less_than", VFunction 2 [] $ \[VInt a, VInt b] k -> k (VBool $ a<b))
  ]

withBuiltins :: IPM r IO a -> IPM r IO a
withBuiltins = do
  localSymbols (
    M.fromList ([
      ("print", VFunction 1 [] printFun)
      ] ++ arithmetic)
    )

mockBuiltins :: Monad m => IPM r m a -> IPM r m a
mockBuiltins = do
  localSymbols (
    M.fromList ([
      ("print", VFunction 1 [] printFunMock)
    ] ++ arithmetic)
    )

printFun :: [Value r IO] -> (Value r IO -> IPM r IO (Value r IO)) -> IPM r IO (Value r IO)
printFun [VInt n] cont = do
  liftIO $ putStrLn (show n)
  cont VNone


printFunMock :: Monad m => [Value r m] -> (Value r m -> IPM r m (Value r m)) -> IPM r m (Value r m)
printFunMock [VInt n] cont = do
  modify $ \s -> s {output= (output s) ++ (show n ++ "\n")}
  cont VNone
