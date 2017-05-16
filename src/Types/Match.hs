module Types.Match (matchType) where

import Types.Core

import Control.Monad.Except
import Data.Map ( (!) )
import qualified Data.Map as M

-- Tries to evaluate values of placeholders
matchType :: Type -> Type -> TCM (M.Map Int Type)
matchType p t = match' p t M.empty

match' :: Type -> Type -> M.Map Int Type -> TCM (M.Map Int Type)
match' _ t ac | not $ null $ getPlaceholders t = throwError "Placeholder in type!"
match' p t ac | p == t = return ac

match' (TPlaceholder n) t ac
  | n `M.member` ac =
      if (ac ! n) == t then return ac
      else throwError $
        "Mismatch: (should be the same)\n"
        ++ show t ++
        "\n-- and --\n"
        ++ show (ac ! n) ++ "\n"
  | otherwise = return $ M.insert n t ac

-- Functions
match' (a :-> b) (c :-> d) ac = match' a c ac >>= match' b d
-- match' (a :-> b) (TVariable n) ac = do
--   constraints <- gets eq_constraints
--   if n `M.member` constraints then
--     match' (a :-> b) (constraints ! n) ac
--   else do
--     t1 <- newTypeVariable
--     t2 <- newTypeVariable
--     addEqConstraint n (t1 :-> t2)
--     match' (a :-> b) (t1 :-> t2) ac
match' (a :-> b) t _ = throwError $ "Type must be function-type:\n" ++ show t ++ "\n"

-- Tuples
match' (TTuple ps) (TTuple ts) ac
  | length ps == length ts = foldM (\a (p, t) -> match' p t a) ac (zip ps ts)
  | otherwise = throwError "Couldn't match tuples with different length"
-- match' (TTuple ps) (TVariable n) ac = do
--   constraints <- gets eq_constraints
--   if n `M.member` constraints then
--     match' (TTuple ps) (constraints!n) ac
--   else do
--     ts <- mapM (\_ -> newTypeVariable) ps
--     addEqConstraint n (TTuple ts)
--     match' (TTuple ps) (TTuple ts) ac
match' (TTuple ps) t _ = throwError $ "Type must be tuple-type:\n" ++ show t ++ "\n"

-- Pointers
match' (TPointer ps) (TPointer ts) ac = match' ps ts ac
match' (TPointer _) t _ = throwError $ "Type must be pointer-type:\n" ++ show t ++ "\n"

match' p t _ = throwError $ "Type mismatch:\n" ++ show p ++ "\n-- and --\n" ++ show t ++ "\n"

getPlaceholders :: Type -> [Int]
getPlaceholders t = getPlaceholders' t []
getPlaceholders' :: Type -> [Int] -> [Int]
getPlaceholders' (TPlaceholder n) ac = if n `elem` ac then [] else [n]
getPlaceholders' TInt _ = []
getPlaceholders' TBool _ = []
getPlaceholders' TVoid _ = []
getPlaceholders' (TPointer t) ac = getPlaceholders' t ac
getPlaceholders' (TVariable _) _ = []
getPlaceholders' (a :-> b) ac =
  let f = getPlaceholders' a ac
  in f ++ getPlaceholders' b (f++ac)
getPlaceholders' (TTuple ts) ac =
  foldr (\e a -> getPlaceholders' e (a++ac) ++ a) [] ts
