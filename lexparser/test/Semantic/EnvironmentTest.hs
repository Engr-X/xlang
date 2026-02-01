module Semantic.EnvironmentTest where

import Test.Tasty
import Test.Tasty.HUnit
import Lex.Token (Token, tokenPos)
import Semantic.Environment
import Util.Type (Position, makePosition)

import qualified Util.Exception as UE 
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Lex.Token as Lex


-- English comment:
-- Shared dummy position used by all test symbols.
pos :: Position
pos = makePosition 0 0 0

mkScope :: [(String, Int)] -> [(String, Int)] -> Scope
mkScope vars funs = Scope {
    scopeId = 0,
    parent  = Nothing,
    sVars = Map.fromList [(n, (i, pos)) | (n, i) <- vars],
    sFuncs = Map.fromList [(n, (i, pos)) | (n, i) <- funs]}

-- English comment:
-- Imported env stores qualified names (QName), not short names.
mkImport :: [(QName, Int)] -> [(QName, Int)] -> ImportEnv
mkImport vars funs = IEnv {
    file = "test",
    iVars = Map.fromList [(q, (i, pos)) | (q, i) <- vars],
    iFuncs = Map.fromList [(q, (i, pos)) | (q, i) <- funs]}

isVarDefineTests :: TestTree
isVarDefineTests = testGroup "Semantic.Environment.isVarDefine" $
    map (\(i, qname, s, envs, out) -> testCase i $ isVarDefine qname s envs @=? out) [
        ("0", ["x"], mkScope [("x", 1)] [], [], True),
        ("1", ["y"], mkScope [] [], [mkImport [(["y"], 2)] []], False),
        ("2", ["pkg","y"], mkScope [] [], [mkImport [(["pkg","y"], 2)] []], True),
        ("3", ["pkg","z"], mkScope [] [], [mkImport [] []], False),
        ("4", ["z"], mkScope [] [], [mkImport [(["z"], 9)] []], False)]


isFunDefineTests :: TestTree
isFunDefineTests = testGroup "Semantic.Environment.isFuncDefine" $ map (\(i, qname, s, envs, out) ->
    testCase i $ isFuncDefine qname s envs @=? out) [
        ("0", ["f"], mkScope [] [("f", 1)], [], True),
        ("1", ["g"], mkScope [] [], [mkImport [] [(["g"], 2)]], False),
        ("2", ["pkg","g"], mkScope [] [], [mkImport [] [(["pkg","g"], 2)]], True),
        ("3", ["pkg","k"], mkScope [] [], [mkImport [] []], False),
        ("4", ["h"], mkScope [] [], [mkImport [] [(["h"], 9)]], False)]


getPackageNameTests :: TestTree
getPackageNameTests = testGroup "Parse.ParserBasic.getPackageName" $ map (\(i, p, ds, out) ->
    testCase i $ getPackageName p ds @=? out) [
        ("0", "stdin", [AST.Import ["java", "util"] toks1], Right []),
        ("1", "stdin", [AST.Package ["a", "b"] toks1, AST.Import ["java", "util"] toks2], Right ["a", "b"]),
        ("2", "stdin", [AST.Package ["a"] toks1, AST.Package ["b"] toks2, AST.Package ["c"] toks3],
            Left [
                UE.Syntax (UE.makeError "C.x" (map tokenPos toks2) UE.multiplePackageMsg),
                UE.Syntax (UE.makeError "C.x" (map tokenPos toks3) UE.multiplePackageMsg)]),

        ("0", "stdin", [
            AST.Import ["java"] toks1,
            AST.Package ["x", "y"] toks2,
            AST.Import ["util"] toks3],
            Right ["x", "y"])]

        where
            toks1, toks2, toks3 :: [Token]
            toks1 = [Lex.Ident "a" pos1]
            toks2 = [Lex.Ident "b" pos2]
            toks3 = [Lex.Ident "c" pos3]

            pos1, pos2, pos3 :: Position
            pos1 = makePosition 1 1 1
            pos2 = makePosition 2 1 1
            pos3 = makePosition 3 1 1



tests :: TestTree
tests = testGroup "Semantic.Environment" [isVarDefineTests, isFunDefineTests]
