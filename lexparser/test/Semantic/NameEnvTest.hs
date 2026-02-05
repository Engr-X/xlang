module Semantic.NameEnvTest where

import Test.Tasty
import Test.Tasty.HUnit
import Lex.Token (Token, tokenPos)
import Semantic.NameEnv
import Util.Type (Position, makePosition)

import qualified Util.Exception as UE 
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Lex.Token as Lex


getPackageNameTests :: TestTree
getPackageNameTests = testGroup "Parse.ParserBasic.getPackageName" $ map (\(i, p, ds, out) ->
    testCase i $ getPackageName p ds @=? out) [
        ("0", "stdin", [AST.Import ["java", "util"] toks1], Right []),
        ("1", "stdin", [AST.Package ["a", "b"] toks1, AST.Import ["java", "util"] toks2], Right ["a", "b"]),
        ("2", "stdin", [AST.Package ["a"] toks1, AST.Package ["b"] toks2, AST.Package ["c"] toks3],
            Left [
                UE.Syntax (UE.makeError "stdin" (map tokenPos toks2) UE.multiplePackageMsg),
                UE.Syntax (UE.makeError "stdin" (map tokenPos toks3) UE.multiplePackageMsg)]),

        ("3", "stdin", [
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


getStateDepthTests :: TestTree
getStateDepthTests = testGroup "Semantic.ContextCheck.getStateDepth" $ map (\(name, st, out) ->
    testCase name $ getStateDepth st @?= out) [
    ("0", CheckState 0 0 [] [] [], 0),
    ("1", CheckState 1 0 [] [] [], 1),
    ("2", CheckState 2 10 [] [] [], 2),
    ("3", CheckState 3 42 [] [] [], 3)]




tests :: TestTree
tests = testGroup "Semantic.NameEnv" [getPackageNameTests, getStateDepthTests]
