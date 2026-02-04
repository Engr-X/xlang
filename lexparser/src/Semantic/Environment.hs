module Semantic.Environment where

import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe)
import Parse.SyntaxTree (Program, Expression, Declaration, declPath)
import Lex.Token (Token, tokenPos)
import Util.Exception (ErrorKind, multiplePackageMsg)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


-- | Stable ids for semantic entities.
type VarId = Int
type FunId = Int
type ScopeId = Int


-- | Fully-qualified name, e.g. ["java","lang","Math"].
type QName = [String]


-- | Control-flow context stack markers used for legality checks.
data CtrlState
    = InFunction
    | InLoop
    | InSwitch
    | InCase
    | InIf
    | InElse
    deriving (Eq, Show)


-- | A single lexical scope (no parent pointer; use the scope stack).
--   Keep only declaration results here for long-term stability.
data Scope = Scope {
    scopeId :: ScopeId,
    sVars :: Map String (VarId, Position),
    sFuncs :: Map (String, [QName]) (FunId, Position)
}
    deriving (Eq, Show)


-- | Mutable checking state for semantic passes.
--   Keep this as a grow-only record to preserve long-term compatibility.
data CheckState = CheckState {
    depth :: Int,             -- ^ Current lexical depth (scopes nesting).
    counter :: Int,           -- ^ Unique id generator for vars/funcs/scopes.
    ctrlStack :: [CtrlState], -- ^ Control-flow context stack.
    scope :: [Scope],         -- ^ Lexical scope stack (top = current).
    classScope :: [Scope]     -- ^ Class/trait scope stack (top = current).
}
    deriving (Eq, Show)


-- | Import environment for a single file.
--   Separate from lexical scopes to keep import rules isolated.
data ImportEnv = IEnv  {
    file :: Path,                                -- ^ File path being checked.
    iVars :: Map QName [Position],               -- ^ Imported vars by qname.
    iFuncs :: Map (QName, [QName]) [Position]    -- ^ Imported funcs by qname + arg types.
}
    deriving (Eq, Show)


-- | Check whether a variable name is defined in any visible scope.
isVarDefine :: QName -> CheckState -> Bool
isVarDefine [] _ = error "input of variable is empty!"
isVarDefine [varName] cs = any (Map.member varName . sVars) (scope cs)
isVarDefine ("this" : [varName]) cs =
    case listToMaybe (classScope cs) of
        Just sj -> Map.member varName (sVars sj)
        Nothing -> False
isVarDefine _ _ = True


isVarImported :: QName -> QName -> [ImportEnv] -> Bool
isVarImported package name = let varName = package ++ name in any (Map.member varName . iVars)



-- | Extract the package name of the current program.
--
-- If no package declaration exists, return an empty package name.
-- If exactly one package declaration exists, return its path.
-- If multiple package declarations exist, report a syntax error
-- for all but the first one.
getPackageName :: Path -> [Declaration] -> Either [ErrorKind] [String]
getPackageName p ds = case filter AST.isPackageDecl ds of
    [] -> Right []
    [decl] -> Right $ declPath decl
    (_:r) -> Left $ map (\x -> UE.Syntax $ UE.makeError p (map tokenPos $ declPos x) multiplePackageMsg) r
    where
        declPos :: Declaration -> [Token]
        declPos (AST.Package _ tokens) = tokens
        declPos (AST.Import _ tokens) = tokens


-- | Get the current lexical depth of the checking state.
--
-- The depth typically represents the nesting level of scopes
-- (e.g. blocks, loops, conditionals).
getStateDepth :: CheckState -> Int
getStateDepth = depth 

