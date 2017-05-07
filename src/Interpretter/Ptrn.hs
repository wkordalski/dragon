module Interpretter.Ptrn where

import Interpretter.Core

import qualified Ast as A
import qualified Data.Map as M

patternMatchValue :: A.Ptrn -> Value r -> IPM r (M.Map String (Value r))
patternMatchValue (A.PNamed s) v = return $ M.singleton s v
patternMatchValue (A.PVoid) v = return M.empty
