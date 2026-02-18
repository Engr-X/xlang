module Semantic.TypeEnvTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv

import qualified Data.Map.Strict as Map


basicClassMapTests :: TestTree
basicClassMapTests = testGroup "Semantic.TypeEnv.basicClassMap" $
    map (\(name, key, expected) -> testCase name $ Map.lookup key basicClassMap @?= expected) [
        ("0", "bool", Just Bool),
        ("1", "int32", Just Int32T),
        ("2", "long", Just Int64T),
        ("3", "unknown", Nothing)]


basicClassEnvTests :: TestTree
basicClassEnvTests = testGroup "Semantic.TypeEnv.basicClassEnv" $
    map (\(name, key, expected) -> testCase name $ Map.lookup key basicClassEnv @?= expected) [
        ("0", ["bool"], Just Bool),
        ("1", ["int64"], Just Int64T),
        ("2", ["float"], Just Float32T),
        ("3", ["foo"], Nothing)]


isBasicClassNameTests :: TestTree
isBasicClassNameTests = testGroup "Semantic.TypeEnv.isBasicClassName" $
    map (\(name, key, expected) -> testCase name $ isBasicClassName key @?= expected) [
        ("0", ["int"], True),
        ("1", ["float64"], True),
        ("2", ["foo"], False),
        ("3", ["pkg", "int"], False)]


normalizeClassTests :: TestTree
normalizeClassTests = testGroup "Semantic.TypeEnv.normalizeClass" $
    map (\(name, input, expected) -> testCase name $ normalizeClass input @?= expected) [
        ("0", Class ["int"] [], Int32T),
        ("1", Array (Class ["int"] []) 2, Array Int32T 2),
        ("2", Class ["List"] [Class ["int"] []], Class ["List"] [Int32T]),
        ("3", Class ["int"] [Class ["int"] []], Class ["int"] [Int32T])]


emptyTypedImportEnvTests :: TestTree
emptyTypedImportEnvTests = testGroup "Semantic.TypeEnv.emptyTypedImportEnv" $
    map (\(name, path) -> testCase name $ emptyTypedImportEnv path @?= TIEnv {
        tFile = path,
        tVars = Map.empty,
        tFuncs = Map.empty
    }) [
        ("0", "a.x"),
        ("1", "src/Main.x"),
        ("2", "module/test"),
        ("3", "")]


tests :: TestTree
tests = testGroup "Semantic.TypeEnv" [
    basicClassMapTests,
    basicClassEnvTests,
    isBasicClassNameTests,
    normalizeClassTests,
    emptyTypedImportEnvTests]
