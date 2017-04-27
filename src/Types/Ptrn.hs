module Types.Ptrn (typeOfNamesInPattern, typeOfNamesInPatterns) where

import Types.Core
import qualified Ast as A

import Control.Monad.Except
import Data.Map ((!))
import qualified Data.Map as M

typeOfNamesInPattern :: A.Ptrn -> Type -> TCM (M.Map String Type)
typeOfNamesInPattern (A.PNamed s) t = return $ M.singleton s t
typeOfNamesInPattern (A.PTuple ps) (TTuple ts) =
  if length ps == length ts then
    foldM (\a (p, t) -> do {er <- typeOfNamesInPattern p t; joinResults er a})
      M.empty (zip ps ts)
  else
    throwError $ "Mismatching tuple sizes."
typeOfNamesInPattern A.PVoid TVoid = return M.empty
typeOfNamesInPattern A.PVoid t =
  throwError $ "Cannot match void pattern match with type:\n" ++ show t ++ "\n"
typeOfNamesInPattern (A.PDereference p) (TPointer t) = typeOfNamesInPattern p t
typeOfNamesInPattern (A.PDereference p) t =
  throwError $ "Cannot match pointer pattern with type:\n" ++ show t ++ "\n"

typeOfNamesInPatterns :: [A.Ptrn] -> Type -> TCM (Type, M.Map String Type)
typeOfNamesInPatterns ps t = foldM reductor (t, M.empty) ps where
  reductor (t, a) e = do
    mr <- matchType (TPlaceholder 1 :-> TPlaceholder 2) t
    pr <- typeOfNamesInPattern e (mr!1)
    rs <- joinResults pr a
    return (mr!2, rs)

joinResults :: (M.Map String Type) -> (M.Map String Type) -> TCM (M.Map String Type)
joinResults m1 m2 =
  if M.null (M.intersection m1 m2) then
    return $ m2 `M.union` m1
  else
    throwError $ "Multiple declaration of name in pattern match"
