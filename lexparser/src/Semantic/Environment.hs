module Semantic.Environment where

import Data.Map.Strict (Map)
import Parse.SyntaxTree (Declaration, declPath)
import Lex.Token (Token, tokenPos)
import Util.Exception (ErrorKind, multiplePackageMsg)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


type VarId = Int
type FunId = Int
type ScopeId = Int


type QName = [String]


data CtrlState
    = InFunction
    | InLoop
    | InSwitch
    | InCase
    | InIf
    | InElse
    deriving (Eq, Show)


data CheckState = CheckState {
    depth :: Int,
    ctrlStack :: [CtrlState],
    scope :: [Scope],
    iEnv :: ImportEnv,
    counter :: Int
}
    deriving (Eq, Show)


data Scope = Scope {
    scopeId :: ScopeId,
    parent :: Maybe ScopeId,
    
    sVars :: Map String (VarId, Position),
    sFuncs :: Map String (FunId, Position)
}
    deriving (Eq, Show)


data ImportEnv = IEnv  {
    file :: Path,
    iVars :: Map QName (VarId, Position),
    iFuncs :: Map QName (FunId, Position)
}
    deriving (Eq, Show)


-- loadImport :: String -> ImportEnv
-- TODO2


-- English comment:
-- If QName is a single segment, it's a short name: look in local scope only.
-- Otherwise treat it as qualified and look in imported environments.
isVarDefine :: QName -> Scope -> [ImportEnv] -> Bool
isVarDefine [name] s _ = Map.member name (sVars s)
isVarDefine qname _ envs = any (Map.member qname . iVars) envs


isFuncDefine :: QName -> Scope -> [ImportEnv] -> Bool
isFuncDefine [name] s _ = Map.member name (sFuncs s)
isFuncDefine qname _ envs = any (Map.member qname . iFuncs) envs


-- get package of this program
getPackageName :: Path -> [Declaration] -> Either [ErrorKind] [String]
getPackageName p ds = case filter AST.isPackageDecl ds of
    [] -> Right []
    [decl] -> Right $ declPath decl
    (_:r) -> Left $ map (\x -> UE.Syntax $ UE.makeError p (map tokenPos $ declPos x) multiplePackageMsg) r
    where
        declPos :: Declaration -> [Token]
        declPos (AST.Package _ tokens) = tokens
        declPos (AST.Import _ tokens) = tokens 
