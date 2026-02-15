module Semantic.TypeEnv where

import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..))
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


-- | Normalize a parsed class into canonical forms.
--   Converts built-in names (e.g. "int") into primitive Class constructors,
--   and recursively normalizes array/generic elements.
normalizeClass :: Class -> Class
normalizeClass cls = case cls of
    Array c n -> Array (normalizeClass c) n
    Class [name] args ->
        case (Map.lookup name basicClassMap, args) of
            (Just prim, []) -> prim
            _ -> Class [name] (map normalizeClass args)
    Class names args -> Class names (map normalizeClass args)
    other -> other


-- | Function signature recorded by the type checker.
data FunSig = FunSig {
    funParams :: [Class],
    funTemplate :: Maybe [Class],
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
    tVars :: Map QName (Class, [Position]),
    tFuncs :: Map QName ([FunSig], [Position])
} deriving (Eq, Show)


emptyTypedImportEnv :: Path -> TypedImportEnv
emptyTypedImportEnv p = TIEnv { tFile = p, tVars = Map.empty, tFuncs = Map.empty }
