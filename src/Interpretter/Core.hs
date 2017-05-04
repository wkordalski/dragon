module Interpretter.Core where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

data IPMEnvironment = IPMEnvironment {
  symbols :: M.Map String Loc,
  allSymbols :: [Loc]   -- for GC
}
data IPMState = IPMState {
  locCounter :: Int,
  memory :: M.Map Loc Value
}

type Loc = Int

data Value
  = VInt Int | VBool Bool | VNone | VTuple [Value]
  -- function is such a thing that gets parameter, continuation (depending on result)
  -- and returns continuation
  | VLReference Loc
  | VFunction (Value -> IPM () IPMCont)

instance Show Value where
  show (VInt n) = "VInt " ++ show n
  show (VBool b) = "VBool " ++ show b
  show (VNone) = "VNone"
  show (VTuple _) = "VTuple ..."
  show (VFunction _) = "VFunction ..."

-- funkcja dostaje parametry, dowiaduje się, co ma zrobić z rezultatem (Value->Cont)
-- wykonuje kod funkcji, któryś statement robi `return $ CReturn wynik`

data IPMCont = CNormal | CReturn Value

ipmEnvironment = IPMEnvironment {
  symbols=M.empty,
  allSymbols=[]
}
ipmState = IPMState {
  locCounter=0,
  memory=M.empty
}

type IPMComplete = StateT IPMState (ReaderT IPMEnvironment (ExceptT String IO))
-- r is type of the result passed to next continuation,
-- a is type of thing passed to return
type IPM r a = ContT r IPMComplete a

--runIPM :: IPM r a -> (ExceptT String IO) (r, IPMState)
runIPM :: (a -> IPMComplete r) -> IPM r a -> (ExceptT String IO) (r, IPMState)
runIPM k m = runReaderT (runStateT (runContT m k) ipmState) ipmEnvironment

askSymbol :: String -> IPM r Loc
askSymbol n = do
  d <- asks symbols
  let (Just a) = n `M.lookup` d
  return a

askMemory :: Loc -> IPM r Value
askMemory l = do
  d <- gets memory
  let (Just v) = l `M.lookup` d
  return v
