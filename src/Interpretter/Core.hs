module Interpretter.Core where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as M

data IPMEnvironment r = IPMEnvironment {
  symbols :: M.Map String Loc,
  -- for GC - the roots
  allSymbols :: [Loc],
  returnCont :: Maybe (Value r -> IPM r (Value r))
}
data IPMState r = IPMState {
  locCounter :: Int,
  memory :: M.Map Loc (Value r)
}

type Loc = Int

data Value r
  = VInt Int | VBool Bool | VNone | VTuple [Value r]
  -- function is such a thing that gets parameter, continuation (depending on result)
  -- and returns continuation
  | VLReference Loc
  -- count of parameters to run, applied parameters, what to call then
  | VFunction Int [Value r] ([Value r] -> IPM r (Value r))
  -- global variable not initialized now - initialization should be done now
  | VUninitialized

instance Show (Value r) where
  show (VInt n) = "VInt " ++ show n
  show (VBool b) = "VBool " ++ show b
  show (VNone) = "VNone"
  show (VTuple _) = "VTuple ..."
  show (VFunction _ _ _) = "VFunction ..."

-- funkcja dostaje parametry, dowiaduje się, co ma zrobić z rezultatem (Value->Cont)
-- wykonuje kod funkcji, któryś statement robi `return $ CReturn wynik`

ipmEnvironment = IPMEnvironment {
  symbols=M.empty,
  allSymbols=[],
  returnCont=Nothing
}
ipmState = IPMState {
  locCounter=0,
  memory=M.empty
}

type IPMComplete r = StateT (IPMState r) (ReaderT (IPMEnvironment r) Identity)
-- r is type of the result after all continuation execution (probably)
-- a is type of thing passed to return
type IPM r a = ExceptT String (WriterT String (ContT r (IPMComplete r))) a
--type IPM r a = ContT r (StateT (IPMState r) (ReaderT (IPMEnvironment r) (Writer String))) a

--type MM r a = (ExceptT String (WriterT String (ContT r (StateT Int (Reader Int))))) a


--runIPM :: IPM r a -> (ExceptT String IO) (r, IPMState)
runIPM :: ((Either String a, String) -> IPMComplete r r) -> IPM r a -> (r, IPMState r)
runIPM k m = runIdentity $ runReaderT (runStateT (runContT (runWriterT (runExceptT m)) k) ipmState) ipmEnvironment
--runIPM k m = runWriter $ runReaderT (runStateT (runContT m k)

askSymbol :: String -> IPM r Loc
askSymbol n = do
  d <- asks symbols
  let (Just a) = n `M.lookup` d
  return a

askMemory :: Loc -> IPM r (Value r)
askMemory l = callCC $ \k -> do
  d <- gets memory
  let (Just v) = l `M.lookup` d
  case v of
    VUninitialized -> throwError $ "Using uninitialized global variable/constant!"
    _ -> k v

askReturnCont :: IPM r (Value r -> IPM r (Value r))
askReturnCont = do
  (Just k) <- asks returnCont
  return k

-- localContinueBreakCont kc kb =
--   local (\e -> e {continueCont=Just kc, breakCont=Just kb})
localSymbols :: (M.Map String (Value r)) -> IPM r a -> IPM r a
localSymbols s m = do
  let symlist = M.toList s
  ss <- mapM (\(n, v) -> do { a <- allocMemory v; return (n, a) }) symlist
  local (\e -> e {
    symbols= (M.fromList ss) `M.union` (symbols e),
    allSymbols= (snd <$> ss) ++ (allSymbols e)
    }) m

allocMemory :: Value r -> IPM r Loc
allocMemory v = do
  a <- gets locCounter
  modify (\s -> s {locCounter=a+1, memory=M.insert a v (memory s)})
  return a

setMemory :: Loc -> Value r -> IPM r ()
setMemory l v = modify (\s -> s {memory=M.insert l v (memory s)})
