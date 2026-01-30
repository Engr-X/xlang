module Semantic.EnvironmentTest where

import Data.Map.Strict (Map)
import Test.Tasty
import Test.Tasty.HUnit
import Semantic.Environment
import Util.Type (Position, makePosition)

import qualified Data.Map.Strict as Map


pos :: Position
pos = makePosition 0 0 0


mkScope :: [(String, Int)] -> [(String, Int)] -> Scope
mkScope vars funs = Scope {
    scopeId = 0,
    parent = Nothing,
    sVars = Map.fromList [(n, (i, pos)) | (n, i) <- vars], 
    sFuncs = Map.fromList [(n, (i, pos)) | (n, i) <- funs]}


mkImport :: [(String, Int)] -> [(String, Int)] -> ImportEnv
mkImport vars funs = IEnv {
    file = "test",
    iVars = Map.fromList [(n, (i, pos)) | (n, i) <- vars],
    iFuncs = Map.fromList [(n, (i, pos)) | (n, i) <- funs]}


isVarDefineTests :: TestTree
isVarDefineTests = testGroup "Semantic.Environment.isVarDefine" $ map (\(i, name, scope, envs, out) ->
    testCase i $ isVarDefine name scope envs @=? out) [
        ("0", "x", mkScope [("x", 1)] [], [], True),
        ("1", "y", mkScope [] [], [mkImport [("y", 2)] []], True),
        ("2", "z", mkScope [("z", 1)] [], [mkImport [("z", 2)] []], True),
        ("3", "a", mkScope [] [], [mkImport [] []], False)]


isFunDefineTests :: TestTree
isFunDefineTests = testGroup "Semantic.Environment.isFuncDefine" $
    map (\(i, name, scope, envs, out) -> testCase i $ isFuncDefine name scope envs @=? out) [
        ("0", "f", mkScope [] [("f", 1)], [], True),
        ("1", "g", mkScope [] [], [mkImport [] [("g", 2)]], True),
        ("2", "h", mkScope [] [("h", 1)], [mkImport [] [("h", 2)]], True),
        ("3", "k", mkScope [] [], [mkImport [] []], False)]


tests :: TestTree
tests = testGroup "Semantic.Environment" [isVarDefineTests, isFunDefineTests]
