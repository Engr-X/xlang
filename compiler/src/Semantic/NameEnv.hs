{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Semantic.NameEnv where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Generics (Generic)
import Lex.Token (Token, tokenPos)
import Parse.SyntaxTree (Declaration, Expression, Statement, declPath)
import Util.Exception (ErrorKind, assignErrorMsg, multiplePackageMsg, unsupportedErrorMsg)
import Util.Type (Path, Position)

import qualified Data.HashSet as HashSet
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
    = InBlock
    | InFunction
    | InLoop
    | InSwitch
    | InCase
    | InIf
    | InElse
    | InClass
    deriving (Eq, Ord, Show, Generic, Hashable)


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
forbiddenMap :: Map CtrlState (HashSet CtrlState)
forbiddenMap =
    Map.fromList
        [ (InBlock, HashSet.fromList [InClass])
        , (InFunction, HashSet.fromList [InClass])
        , (InLoop, HashSet.fromList [InClass])
        , (InSwitch, HashSet.fromList [InBlock, InFunction, InLoop, InSwitch, InIf, InElse, InClass])
        , (InCase, HashSet.fromList [InClass])
        , (InIf, HashSet.fromList [InClass])
        , (InElse, HashSet.fromList [InClass])
        , (InClass, HashSet.fromList [InBlock, InLoop, InSwitch, InCase, InIf, InElse])
        ]


-- | Check whether a parent control state forbids a child control state.
forbiddenFor :: Maybe CtrlState -> CtrlState -> Bool
forbiddenFor Nothing _ = False
forbiddenFor (Just parent) current = HashSet.member current (Map.findWithDefault HashSet.empty parent forbiddenMap)


-- | A single lexical scope (no parent pointer; use the scope stack).
data Scope = Scope {
    scopeId :: ScopeId,
    sVars :: Map String (VarId, Position),
    sFuncs :: Map QName [Position]
    }
    deriving (Eq, Show)


-- | Mutable checking state for semantic passes.
data CheckState = CheckState {
    depth :: Int,
    varCounter :: Int,
    scopeCounter :: Int,
    ctrlStack :: [CtrlState],
    scope :: [Scope],
    classScope :: [Scope]
    }
    deriving (Eq, Show)


-- | Lookup a variable id (and its def position) from the current scope stack.
lookupVarId :: String -> CheckState -> Maybe (VarId, Position)
lookupVarId name st = listToMaybe $ mapMaybe (Map.lookup name . sVars) (scope st)


-- | Import NameEnv for a single file.
data ImportEnv = IEnv {
    file :: Path,
    iVars :: Map QName [Position],
    iFuncs :: Map QName [Position]}
    deriving (Eq, Show)


emptyImportEnv :: Path -> ImportEnv
emptyImportEnv p = IEnv {file = p, iVars = Map.empty, iFuncs = Map.empty}


defaultImportEnv :: Path -> ImportEnv
defaultImportEnv p = envWithDefaultImport (emptyImportEnv p)
    where
        envWithDefaultImport :: ImportEnv -> ImportEnv
        envWithDefaultImport env =
            let addFun qn env0 = env0 {iFuncs = Map.insert qn [] (iFuncs env0)}
                addPut env0 = addFun ["xlang", "io", "put"] (addFun ["put"] env0)
                addPutln env0 = addFun ["xlang", "io", "putln"] (addFun ["putln"] env0)
            in addPutln (addPut env)


-- | Hidden import key prefix used for non-public exported symbols.
hiddenQNamePrefix :: String
hiddenQNamePrefix = "$hidden$"


-- | Encode a hidden symbol key into the regular import table.
toHiddenQName :: QName -> QName
toHiddenQName q = hiddenQNamePrefix : q


-- | Define a local variable from assignment like: x = expr.
defineLocalVar :: Path -> Expression -> CheckState -> Either ErrorKind CheckState
defineLocalVar p (AST.Binary AST.Assign lhs _ _) st = case lhs of
    AST.Variable name nameTok -> case scope st of
        [] -> error "internal error: no scope while defining variable"
        (sc : rest) ->
            if isVarDefine name st
                then Right st
                else
                    let vid = varCounter st
                        sc' = sc {sVars = Map.insert name (vid, tokenPos nameTok) (sVars sc)}
                    in Right $ st {varCounter = succ vid, scope = sc' : rest}
    AST.Qualified _ tokens -> Left $ UE.Syntax (UE.makeError p (map tokenPos tokens) assignErrorMsg)
    _ -> error "internal error: assign lhs must be variable"
defineLocalVar _ _ st = Right st


-- | Define a variable in the current lexical scope via explicit declaration.
defineDeclaredVar :: Path -> String -> Token -> CheckState -> Either ErrorKind CheckState
defineDeclaredVar p name nameTok st = case scope st of
    [] -> error "internal error: no scope while defining variable"
    (sc : rest) ->
        if Map.member name (sVars sc)
            then Left $ UE.Syntax $ UE.makeError p [tokenPos nameTok] (UE.multipleVariableDefMsg name)
            else
                let vid = varCounter st
                    sc' = sc {sVars = Map.insert name (vid, tokenPos nameTok) (sVars sc)}
                in Right $ st {varCounter = succ vid, scope = sc' : rest}


-- | Define a function name in the current scope.
defineFunc :: Path -> Statement -> CheckState -> Either ErrorKind CheckState
defineFunc p stmt st = case functionNameExpr stmt of
    Nothing -> Right st
    Just (AST.Variable name tok) -> defineFuncByName name tok st
    Just (AST.Qualified _ toks) -> Left $ UE.Syntax (UE.makeError p (map tokenPos toks) unsupportedErrorMsg)
    Just _ -> Right st
    where
        defineFuncByName :: String -> Token -> CheckState -> Either ErrorKind CheckState
        defineFuncByName name tok st0 = case scope st0 of
            [] -> error "internal error: no scope while defining function"
            (sc : rest) ->
                if Map.member [name] (sFuncs sc)
                    then Right st0
                    else
                        let sc' = sc {sFuncs = Map.insert [name] [tokenPos tok] (sFuncs sc)}
                        in Right $ st0 {scope = sc' : rest}


-- | Get function name expression from function-like statements.
functionNameExpr :: Statement -> Maybe Expression
functionNameExpr (AST.Function _ nameExpr _ _) = Just nameExpr
functionNameExpr (AST.FunctionT _ nameExpr _ _ _) = Just nameExpr
functionNameExpr _ = Nothing


-- | Check whether a variable name is defined in any active lexical scope.
isVarDefine :: String -> CheckState -> Bool
isVarDefine varName = any (Map.member varName . sVars) . scope


-- | Check whether a variable name is imported in any import environment.
isVarImport :: QName -> [ImportEnv] -> Bool
isVarImport varName = any (Map.member varName . iVars)


-- | Lookup hidden (non-public) imported variable positions.
lookupHiddenVarPos :: QName -> [ImportEnv] -> Maybe [Position]
lookupHiddenVarPos varName envs = listToMaybe $ mapMaybe (Map.lookup (toHiddenQName varName) . iVars) envs


-- | Check whether a function name is defined in any active lexical scope.
isFuncDefine :: QName -> CheckState -> Bool
isFuncDefine funName = any (Map.member funName . sFuncs) . scope


-- | Check whether a function name is imported in any import environment.
isFunImport :: QName -> [ImportEnv] -> Bool
isFunImport funName = any (Map.member funName . iFuncs)


-- | Lookup hidden (non-public) imported function positions.
lookupHiddenFunPos :: QName -> [ImportEnv] -> Maybe [Position]
lookupHiddenFunPos funName envs = listToMaybe $ mapMaybe (Map.lookup (toHiddenQName funName) . iFuncs) envs


-- | Extract the package name of the current program.
getPackageName :: Path -> [Declaration] -> Either [ErrorKind] [String]
getPackageName p ds = case filter AST.isPackageDecl ds of
    [] -> Right []
    [decl] -> Right $ declPath decl
    (_ : r) -> Left $ map (\x -> UE.Syntax $ UE.makeError p (map tokenPos $ declPos x) multiplePackageMsg) r
    where
        declPos :: Declaration -> [Token]
        declPos (AST.Package _ tokens) = tokens
        declPos (AST.Import _ tokens) = tokens
        declPos (AST.JavaName _ token) = [token]


-- | Get the current lexical depth.
getStateDepth :: CheckState -> Int
getStateDepth = depth

