module Semantic.Environment where

import Data.Map.Strict (Map)
import Lex.Token (Token)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map


type VarId = Int
type FunId = Int
type ScopeId = Int


data Scope = Scope {
    scopeId   :: ScopeId,
    parent    :: Maybe ScopeId,
    
    sVars :: Map String (VarId, Position),
    sFuncs :: Map String (FunId, Position)
}
    deriving (Eq, Show)


data ImportEnv = IEnv  {
    file :: Path,
    iVars :: Map String (VarId, Position),
    iFuncs :: Map String (FunId, Position)
}
    deriving (Eq, Show)


-- loadImport :: String -> ImportEnv
-- TODO2


-- | Check whether a variable is defined.
-- A variable is considered defined if:
--   1. It exists in the current scope, or
--   2. It exists in any imported environment.
isVarDefine :: String -> Scope -> [ImportEnv] -> Bool
isVarDefine name scope envs = Map.member name (sVars scope) || any (Map.member name . iVars) envs


-- | Check whether a function is defined.
-- A function is considered defined if:
--   1. It exists in the current scope, or
--   2. It exists in any imported environment.
isFuncDefine :: String -> Scope -> [ImportEnv] -> Bool
isFuncDefine name scope envs = Map.member name (sFuncs scope) || any (Map.member name . iFuncs) envs

