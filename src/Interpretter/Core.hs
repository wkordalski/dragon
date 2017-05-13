module Interpretter.Core where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS

import qualified Data.Map as M

data IPMEnvironment r m = IPMEnvironment {
  symbols :: M.Map String Loc,
  -- for GC - the roots
  allSymbols :: [Loc],
  returnCont :: Maybe (Value r m -> IPM r m (Value r m))
}
data IPMState r m = IPMState {
  locCounter :: Int,
  memory :: M.Map Loc (Value r m),
  output :: String
}

type Loc = Int

data Value r m
  = VInt Int | VBool Bool | VNone | VTuple [Value r m]
  -- function is such a thing that gets parameter, continuation (depending on result)
  -- and returns continuation
  | VLReference Loc
  | VPointer Loc
  -- count of parameters to run, applied parameters, what to call then
  | VFunction Int [Value r m] ([Value r m] -> (Value r m -> IPM r m (Value r m)) -> IPM r m (Value r m))
  -- global variable not initialized now - initialization should be done now
  | VUninitialized

instance Show (Value r m) where
  show (VInt n) = "VInt " ++ show n
  show (VBool b) = "VBool " ++ show b
  show (VNone) = "VNone"
  show (VTuple _) = "VTuple ..."
  show (VFunction _ _ _) = "VFunction ..."
  show (VUninitialized) = "<uninitialized value>"
  show (VPointer l) = "<pointer " ++ show l ++ ">"
  show (VLReference l) = "<l-reference " ++ show l ++ ">"

-- funkcja dostaje parametry, dowiaduje się, co ma zrobić z rezultatem (Value->Cont)
-- wykonuje kod funkcji, któryś statement robi `return $ CReturn wynik`

ipmEnvironment = IPMEnvironment {
  symbols=M.empty,
  allSymbols=[],
  returnCont=Nothing
}
ipmState = IPMState {
  locCounter=0,
  memory=M.empty,
  output=""
}

-- r is type of the result after all continuation execution (probably)
-- a is type of thing passed to return
type IPM r m a = ExceptT String (ReaderT (IPMEnvironment r m) (ContT r (StateT (IPMState r m) m))) a
--data IPMGeneric a where
--  IPMGeneric :: (forall r m. IPM r m a) -> IPMGeneric a
--type IPM r a = ContT r (StateT (IPMState r) (ReaderT (IPMEnvironment r) (Writer String))) a

--type MM r a = (ExceptT String (WriterT String (ContT r (StateT Int (Reader Int))))) a


--runIPM :: ((Either String a, String) -> IPMComplete r r) -> IPM r a -> (r, IPMState r)
runIPM :: (Either String a -> (StateT (IPMState r m) m) r) -> IPM r m a -> m (r, IPMState r m)
--runIPM k m = runIdentity $ runReaderT (runStateT (runContT (runWriterT (runExceptT m)) k) ipmState) ipmEnvironment
runIPM k m = runStateT (runContT (runReaderT (runExceptT m) ipmEnvironment) k) ipmState

askSymbol :: String -> IPM r m Loc
askSymbol n = do
  d <- asks symbols
  let (Just a) = n `M.lookup` d
  return a

askMemory :: Monad m => Loc -> IPM r m (Value r m)
askMemory l = callCC $ \k -> do
  d <- gets memory
  let (Just v) = l `M.lookup` d
  case v of
    VUninitialized -> throwError $ "Using uninitialized global variable/constant!"
    _ -> k v

askReturnCont :: IPM r m (Value r m -> IPM r m (Value r m))
askReturnCont = do
  (Just k) <- asks returnCont
  return k

localSymbols :: Monad m => (M.Map String (Value r m)) -> IPM r m a -> IPM r m a
localSymbols s m = do
  let symlist = M.toList s
  ss <- mapM (\(n, v) -> do { a <- allocMemory v; return (n, a) }) symlist
  local (\e -> e {
    symbols= (M.fromList ss) `M.union` (symbols e),
    allSymbols= (snd <$> ss) ++ (allSymbols e)
    }) m

allocMemory :: Monad m => Value r m -> IPM r m Loc
allocMemory (VLReference l) = return l
allocMemory v = do
  a <- gets locCounter
  modify (\s -> s {locCounter=a+1, memory=M.insert a v (memory s)})
  return a

setMemory :: Monad m => Loc -> Value r m -> IPM r m ()
setMemory l v = do
  vv <- unreference v
  modify (\s -> s {memory=M.insert l vv (memory s)})

unreference :: Monad m => Value r m -> IPM r m (Value r m)
unreference (VLReference l) = do
  v <- askMemory l >>= unreference
  return v
unreference v = return v
