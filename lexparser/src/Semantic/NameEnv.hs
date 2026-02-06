{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Semantic.NameEnv where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Parse.SyntaxTree (Expression, Statement, Declaration, declPath)
import Lex.Token (Token, tokenPos)
import Util.Exception (ErrorKind, multiplePackageMsg, assignErrorMsg, unsupportedErrorMsg)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet
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
    = InBlock
    | InFunction
    | InLoop
    | InSwitch
    | InCase
    | InIf
    | InElse
    | InClass
    deriving (Eq, Ord, Show, Generic, Hashable)


-- | better to String function for state
prettyCtrlState :: CtrlState -> String 
prettyCtrlState InBlock = "block"
prettyCtrlState InFunction = "function"
prettyCtrlState InLoop = "loop"
prettyCtrlState InSwitch = "switch"
prettyCtrlState InCase = "case"
prettyCtrlState InIf = "if"
prettyCtrlState InElse = "else"
prettyCtrlState InClass = "class"


-- | Forbidden inner control states for a given outer state.
--   read as if key is parrent, value cannot be it children
forbiddenMap :: Map CtrlState (HashSet CtrlState)
forbiddenMap = Map.fromList [
    (InBlock, HashSet.fromList [InClass]),
    (InFunction, HashSet.fromList [InClass]),
    (InLoop, HashSet.fromList [InClass]),
    (InSwitch, HashSet.fromList [InBlock, InFunction, InLoop, InSwitch, InIf, InElse, InClass]),
    (InCase, HashSet.fromList [InClass]),
    (InIf, HashSet.fromList [InClass]),
    (InElse, HashSet.fromList [InClass]),
    (InClass, HashSet.fromList [InBlock, InLoop, InSwitch, InCase, InIf, InElse])]


-- | Check whether a parent control state forbids a child control state.
forbiddenFor :: CtrlState -> CtrlState -> Bool
forbiddenFor parent current = HashSet.member current (Map.findWithDefault HashSet.empty parent forbiddenMap)


-- | A single lexical scope (no parent pointer; use the scope stack).
--   Keep only declaration results here for long-term stability.
data Scope = Scope {
    scopeId :: ScopeId,
    sVars :: Map String (VarId, Position),
    sFuncs :: Map QName [Position]
}
    deriving (Eq, Show)


-- | Mutable checking state for semantic passes.
--   Keep this as a grow-only record to preserve long-term compatibility.
data CheckState = CheckState {
    depth :: Int,             -- ^ Current lexical depth (scopes nesting).
    varCounter :: Int,        -- ^ Unique id generator for vars.
    scopeCounter :: Int,      -- ^ Unique id generator for scopes.
    ctrlStack :: [CtrlState], -- ^ Control-flow context stack.
    scope :: [Scope],         -- ^ Lexical scope stack (top = current).
    classScope :: [Scope]     -- ^ Class/trait scope stack (top = current).
}
    deriving (Eq, Show)


-- | Import NameEnv for a single file.
--   Separate from lexical scopes to keep import rules isolated.
data ImportEnv = IEnv  {
    file :: Path,                                -- ^ File path being checked.
    iVars :: Map QName [Position],               -- ^ Imported vars by qname.
    iFuncs :: Map QName [Position]                -- ^ Imported funcs by qname + arg types.
}
    deriving (Eq, Show)


-- Define a local variable when we see a top-level assignment like: x = expr.
-- This only treats "Variable" as a definable l-value for now.
defineLocalVar :: Path -> Expression -> CheckState -> Either ErrorKind CheckState
defineLocalVar p (AST.Binary AST.Assign lhs _ _) st = case lhs of
    AST.Variable name nameTok ->
        case scope st of
            [] -> error "internal error: there is no scope to define !!! while define variable"
            (sc:rest) ->
                if Map.member name (sVars sc) then Right st
                else 
                    let vid = varCounter st
                        sc' = sc { sVars = Map.insert name (vid, tokenPos nameTok) (sVars sc) }
                    in Right $ st { varCounter = succ vid, scope = sc' : rest}
    AST.Qualified _ tokens -> Left $ UE.Syntax (UE.makeError p (map tokenPos tokens) assignErrorMsg)
    _ -> error "internal error this error should be catched in process of parser"
defineLocalVar _ _ st = Right st


-- | Define a function name in the current scope.
--   For qualified names, report unsupported syntax.
--   Non-function statements are ignored.
defineFunc :: Path -> Statement -> CheckState -> Either ErrorKind CheckState
defineFunc _ (AST.Function _ (AST.Variable name token) _ _ _) st = case scope st of
    [] -> error "internal error: there is no scope to define !!! while define function"
    (sc:rest) -> 
        if Map.member [name] (sFuncs sc) then Right st
        else let sc' = sc { sFuncs = Map.insert [name] [tokenPos token] (sFuncs sc) } 
             in Right $ st { scope = sc' : rest}
defineFunc p (AST.Function _ (AST.Qualified _ tokens) _ _ _) _ = Left $ UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg)
defineFunc _ _ st = Right st



-- | Check whether a variable name is defined in any active lexical scope.
isVarDefine :: String -> CheckState -> Bool
isVarDefine varName = any (Map.member varName . sVars) . scope


-- | Check whether a variable name is imported in any import environment.
isVarImport :: QName -> [ImportEnv] -> Bool
isVarImport varName = any (Map.member varName . iVars)


-- | Check whether a function name is defined in any active lexical scope.
isFuncDefine :: QName -> CheckState -> Bool
isFuncDefine funName = any (Map.member funName . sFuncs) . scope

-- | Check whether a function name is imported in any import environment.
isFunImport :: QName -> [ImportEnv] -> Bool
isFunImport funName = any (Map.member funName . iFuncs)




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

