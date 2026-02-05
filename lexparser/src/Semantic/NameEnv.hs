module Semantic.NameEnv where

import Data.Map.Strict (Map)
import Parse.SyntaxTree (Expression, Statement, Declaration, declPath)
import Lex.Token (Token, tokenPos)
import Util.Exception (ErrorKind, multiplePackageMsg, assignErrorMsg, unsupportedErrorMsg)
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
    | InClass
    deriving (Eq, Show)


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
    counter :: Int,           -- ^ Unique id generator for vars/funcs/scopes.
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


-- English comment:
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
                    let vid = counter st
                        sc' = sc { sVars = Map.insert name (vid, tokenPos nameTok) (sVars sc) }
                    in Right $ st { counter = succ vid, scope = sc' : rest}
    AST.Qualified _ tokens -> Left $ UE.Syntax (UE.makeError p (map tokenPos tokens) assignErrorMsg)
    _ -> error "internal error this error should be catched in process of parser"
defineLocalVar _ _ st = Right st


defineFunc :: Path -> Statement -> CheckState -> Either ErrorKind CheckState
defineFunc p (AST.Function _ (AST.Variable name token) _ _ _) st = case scope st of
    [] -> error "internal error: there is no scope to define !!! while define function"
    (sc:rest) -> 
        if Map.member [name] (sFuncs sc) then Right st
        else let sc' = sc { sFuncs = Map.insert [name] [tokenPos token] (sFuncs sc) } 
             in Right $ st { scope = sc' : rest}
defineFunc p (AST.Function _ (AST.Qualified name tokens) _ _ _) st = Left $ UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg)



isVarDefine :: String -> CheckState -> Bool
isVarDefine varName = any (Map.member varName . sVars) . scope


isVarImport :: QName -> [ImportEnv] -> Bool
isVarImport varName = any (Map.member varName . iVars)


isFuncDefine :: QName -> CheckState -> Bool
isFuncDefine funName = any (Map.member funName . sFuncs) . scope

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

