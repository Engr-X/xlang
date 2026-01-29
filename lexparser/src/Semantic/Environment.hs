module Semantic.Environment where

import Data.Map.Strict (Map)
import Lex.Token (Token)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map


type VarId = Int
type FunId = Int
type ScopeId = Int


data Scope = Scope
{
    scopeId   :: ScopeId,
    parent    :: Maybe ScopeId,
    
    variables :: Map String (VarId, Position),
    functions :: Map String (FunId, Position)
}
deriving (Eq, Show)


data ImportEnv = IEnv 
{
    file      :: Path,
    variables :: Map String (VarId, Position),
    functions :: Map String (FunId, Position)
}
deriving (Eq, Show)
