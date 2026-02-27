module Semantic.TypeEnv where

import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..))
import Parse.ParserBasic (DeclFlags, Decl)
import Semantic.NameEnv (QName, VarId)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map


-- | Built-in class aliases (unqualified names).
basicClassMap :: Map String Class
basicClassMap = Map.fromList [
    ("bool", Bool),
    ("char", Char),
    ("byte", Int8T), ("int8", Int8T),
    ("short", Int16T), ("int16", Int16T),
    ("int", Int32T), ("int32", Int32T),
    ("long", Int64T), ("int64", Int64T),
    ("float", Float32T), ("float32", Float32T),
    ("double", Float64T), ("float64", Float64T),
    ("float128", Float128T),
    ("void", Void)]


-- | Built-in class aliases keyed by qualified name.
basicClassEnv :: Map QName Class
basicClassEnv = Map.fromList $ map (\(k, v) -> ([k], v)) (Map.toList basicClassMap)


-- | Check whether a qualified name is a built-in class name.
isBasicClassName :: QName -> Bool
isBasicClassName q = Map.member q basicClassEnv


-- | Function signature recorded by the type checker.
data FunSig = FunSig {
    funParams :: [Class],
    funReturn :: Class
} deriving (Eq, Show)


-- | Mapping from variable id to its inferred/declared type + def position.
--   We keep Position so a variable can be traced even if names repeat.
type VarTable = Map VarId (Class, Position)


-- | Mapping from function name to overload set.
type FunTable = Map QName [FunSig]


-- | Typed import environment for type checking.
--   Keeps types + positions for imported symbols.
data TypedImportEnv = TIEnv {
    tFile :: Path,
    -- | key: imported name as written (possibly short); value carries full qname.
    tVars :: Map QName (Class, [Position], QName),
    tFuncs :: Map QName ([FunSig], [Position], QName)
} deriving (Eq, Show)


-- | Full variable usage table (local or imported).
data FullVarTable
    = VarLocal Decl String VarId
    | VarImported DeclFlags Class QName
    deriving (Eq, Show)
    

-- | Full function usage table (local or imported).
data FullFunctionTable
    = FunLocal Decl String FunSig
    | FunImported DeclFlags QName FunSig
    deriving (Eq, Show)


emptyTypedImportEnv :: Path -> TypedImportEnv
emptyTypedImportEnv p = TIEnv { tFile = p, tVars = Map.empty, tFuncs = Map.empty }

-- | Default typed import environment with built-in functions preloaded.
defaultTypedImportEnv :: Path -> TypedImportEnv
defaultTypedImportEnv p = envWithDefault (emptyTypedImportEnv p)


-- | Add a default imported function with void return type.
--   key: short name used in code; qname: fully-qualified target.
addDefaultFun :: QName -> (Class, [Class]) -> QName -> TypedImportEnv -> TypedImportEnv
addDefaultFun name (retT, args) full env =
    let sig = FunSig { funParams = args, funReturn = retT }
        entry = ([sig], [], full)
        merge (newSigs, newPos, newQName) (oldSigs, oldPos, oldQName) =
            let sigs = oldSigs ++ filter (`notElem` oldSigs) newSigs
                pos = oldPos ++ newPos
                qn = if null oldQName then newQName else oldQName
            in (sigs, pos, qn)
        env1 = env { tFuncs = Map.insertWith merge name entry (tFuncs env) }
    in if name == full
        then env1
        else env1 { tFuncs = Map.insertWith merge full entry (tFuncs env1) }


envWithDefault :: TypedImportEnv -> TypedImportEnv
envWithDefault env = let pTypes = [Int8T, Int16T, Int32T, Int64T, Float32T, Float64T, Float128T, Bool, Char]
    in addPutln pTypes (addPut pTypes env)

    where
        addPut :: [Class] -> TypedImportEnv -> TypedImportEnv
        addPut cs env0 = foldl (\e x -> addDefaultFun ["put"] (Void, [x]) ["xlang", "io", "put"] e) env0 cs

        addPutln :: [Class] -> TypedImportEnv -> TypedImportEnv
        addPutln cs env0 = foldl (\e x -> addDefaultFun ["putln"] (Void, [x]) ["xlang", "io", "putln"] e) env0 cs
