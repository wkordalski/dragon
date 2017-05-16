module Types.Core (
  Type (..), TCM, runTCM, typeFromAst,
  askTypeOf, askReturnType, askAutoReturn,
  localTypeOf, localTypesOf, localFunction, localNoAutoReturn,
  newTypeVariable) where

import qualified Ast as A

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map ((!))
import qualified Data.Map as M

infixr 5 :->

data Type
  = TInt | TBool | TVoid
  | TPointer Type
  | Type :-> Type
  | TTuple [Type]
  | TVariable Int           -- In the future for type reconstruction
  | TPlaceholder Int        -- Used for querying match function
  deriving (Eq, Show)       -- TODO: custom Eq when type reconstruction

data TCMState = TCMState {
  tvar_counter :: Int
} deriving (Show, Eq)

tcmState = TCMState { tvar_counter=0 }

data TCMEnvironment = TCMEnvironment {
  name_typing :: M.Map String Type,
  return_type :: Maybe Type,
  auto_return :: Bool
}

tcmEnvironment = TCMEnvironment {
  name_typing=M.empty,
  return_type=Nothing,
  auto_return=False
}

-- Type Checking Monad
type TCM a = StateT TCMState (ReaderT TCMEnvironment (Except String)) a

runTCM m = runReaderT (runStateT m tcmState) tcmEnvironment

typeFromAst :: A.TypeExpr -> TCM Type
typeFromAst A.TInt = return TInt
typeFromAst A.TBool = return TBool
typeFromAst A.TVoid = return TVoid
typeFromAst (A.TPointer t) = do
  t' <- typeFromAst t
  return $ TPointer t'
typeFromAst (A.TTuple ts) = do
  ts' <- (mapM typeFromAst ts)
  return $ TTuple ts'
typeFromAst (A.TFunction a b) = liftM2 (:->) (typeFromAst a) (typeFromAst b)

typeFromAst (A.TNamed _) = throwError "Named types not supported yet."
typeFromAst (A.TUnknown) = throwError "Type reconstruction not supported yet."


askTypeOf :: String -> TCM Type
askTypeOf s = do
  typing <- asks name_typing
  if s `M.member` typing then
    return $ typing ! s
  else
    throwError $ "Unknown identifier " ++ s ++ "\n"

askReturnType :: TCM Type
askReturnType = do
  rt <- asks return_type
  case rt of
    Just t -> return t
    Nothing -> throwError $ "No return type outside function!"


askAutoReturn :: TCM Bool
askAutoReturn = asks auto_return

localTypeOf :: String -> Type -> TCM a -> TCM a
localTypeOf s t = local (\e -> e {name_typing=M.insert s t (name_typing e)})

localTypesOf :: (M.Map String Type) -> TCM a -> TCM a
localTypesOf s = local (\e -> e {name_typing=s `M.union` (name_typing e)})

localFunction :: (M.Map String Type) -> (M.Map String Type) -> Type -> TCM a -> TCM a
localFunction sm mta tr = local applyFunc where
  applyFunc e =
    e {
      name_typing=mta `M.union` (sm `M.union` name_typing e),
      return_type=Just tr,
      auto_return=True
    }

localNoAutoReturn :: TCM a -> TCM a
localNoAutoReturn = local (\e -> e {auto_return=False})

newTypeVariable :: TCM Type
newTypeVariable = do
  idx <- gets tvar_counter
  modify (\s -> s {tvar_counter=idx+1})
  return $ TVariable idx
