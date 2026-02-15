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


forbiddenForTests :: TestTree
forbiddenForTests = testGroup "Semantic.NameEnv.forbiddenFor" $ map (\(name, parent, current, out) ->
    testCase name $ forbiddenFor parent current @?= out) [
    ("0", InBlock, InClass, True),
    ("1", InSwitch, InIf, True),
    ("2", InBlock, InLoop, False),
    ("3", InFunction, InLoop, False)]


defineLocalVarTests :: TestTree
defineLocalVarTests = testGroup "Semantic.NameEnv.defineLocalVar" $ map (\(name, expr, st, out) ->
    testCase name $ defineLocalVar "stdin" expr st @?= out) [
    ("0", assignVarX, stBase, Right stAfterNew),
    ("1", assignVarX, stExisting, Right stExisting),
    ("2", assignQualified, stBase, Left qualifiedErr),
    ("3", nonAssignExpr, stBase, Right stBase)]
    where
        pos1, pos2, pos3 :: Position
        pos1 = makePosition 1 1 1
        pos2 = makePosition 1 3 1
        pos3 = makePosition 1 5 1

        tokX, tokA, tokB, tokAssign, tokNum :: Token
        tokX = Lex.Ident "x" pos1
        tokA = Lex.Ident "a" pos1
        tokB = Lex.Ident "b" pos2
        tokAssign = Lex.Symbol Lex.Assign pos2
        tokNum = Lex.NumberConst "1" pos3

        rhsConst :: AST.Expression
        rhsConst = AST.IntConst "1" tokNum

        assignVarX :: AST.Expression
        assignVarX = AST.Binary AST.Assign (AST.Variable "x" tokX) rhsConst tokAssign

        assignQualified :: AST.Expression
        assignQualified = AST.Binary AST.Assign (AST.Qualified ["a", "b"] [tokA, tokB]) rhsConst tokAssign

        nonAssignExpr :: AST.Expression
        nonAssignExpr = rhsConst

        emptyScope :: Scope
        emptyScope = Scope { scopeId = 0, sVars = Map.empty, sFuncs = Map.empty }

        stBase :: CheckState
        stBase = CheckState 0 0 0 [] [emptyScope] []

        stAfterNew :: CheckState
        stAfterNew = stBase {
            varCounter = 1,
            scope = [emptyScope { sVars = Map.insert "x" (0, pos1) Map.empty }]
        }

        stExisting :: CheckState
        stExisting = CheckState 0 10 0 [] [emptyScope { sVars = Map.insert "x" (5, pos1) Map.empty }] []

        qualifiedErr :: UE.ErrorKind
        qualifiedErr = UE.Syntax $ UE.makeError "stdin" (map tokenPos [tokA, tokB]) UE.assignErrorMsg


defineFuncTests :: TestTree
defineFuncTests = testGroup "Semantic.NameEnv.defineFunc" $ map (\(name, stmt, st, out) ->
    testCase name $ defineFunc "stdin" stmt st @?= out) [
    ("0", funStmtX, stBase, Right stAfterNew),
    ("1", funStmtX, stExisting, Right stExisting),
    ("2", funStmtQualified, stBase, Left qualifiedErr),
    ("3", nonFuncStmt, stBase, Right stBase)]
    where
        pos1, pos2, pos3 :: Position
        pos1 = makePosition 1 1 1
        pos2 = makePosition 1 3 1
        pos3 = makePosition 1 5 1

        tokX, tokA, tokB, tokNum :: Token
        tokX = Lex.Ident "x" pos1
        tokA = Lex.Ident "a" pos1
        tokB = Lex.Ident "b" pos2
        tokNum = Lex.NumberConst "1" pos3

        emptyScope :: Scope
        emptyScope = Scope { scopeId = 0, sVars = Map.empty, sFuncs = Map.empty }

        stBase :: CheckState
        stBase = CheckState 0 0 0 [] [emptyScope] []

        stAfterNew :: CheckState
        stAfterNew = stBase {
            scope = [emptyScope { sFuncs = Map.insert ["x"] [pos1] Map.empty }]
        }

        stExisting :: CheckState
        stExisting = CheckState 0 0 0 [] [emptyScope { sFuncs = Map.insert ["x"] [pos1] Map.empty }] []

        funStmtX :: AST.Statement
        funStmtX = AST.Function (AST.Int32T, []) (AST.Variable "x" tokX) [] (AST.Multiple [])

        funStmtQualified :: AST.Statement
        funStmtQualified =
            AST.Function (AST.Int32T, []) (AST.Qualified ["a", "b"] [tokA, tokB]) [] (AST.Multiple [])

        nonFuncStmt :: AST.Statement
        nonFuncStmt = AST.Expr (AST.IntConst "1" tokNum)

        qualifiedErr :: UE.ErrorKind
        qualifiedErr = UE.Syntax $ UE.makeError "stdin" (map tokenPos [tokA, tokB]) UE.unsupportedErrorMsg


isVarDefineTests :: TestTree
isVarDefineTests = testGroup "Semantic.NameEnv.isVarDefine" $ map (\(name, varName, st, out) ->
    testCase name $ isVarDefine varName st @?= out) [
    ("0", "x", stWithX, True),
    ("1", "y", stWithYOuter, True),
    ("2", "z", stWithX, False),
    ("3", "x", stEmpty, False)]
    where
        pos1, pos2 :: Position
        pos1 = makePosition 1 1 1
        pos2 = makePosition 2 1 1

        scopeTop :: Scope
        scopeTop = Scope { scopeId = 0, sVars = Map.fromList [("x", (1, pos1))], sFuncs = Map.empty }

        scopeOuter :: Scope
        scopeOuter = Scope { scopeId = 1, sVars = Map.fromList [("y", (2, pos2))], sFuncs = Map.empty }

        stWithX :: CheckState
        stWithX = CheckState 0 0 0 [] [scopeTop] []

        stWithYOuter :: CheckState
        stWithYOuter = CheckState 0 0 0 [] [scopeTop, scopeOuter] []

        stEmpty :: CheckState
        stEmpty = CheckState 0 0 0 [] [] []


isVarImportTests :: TestTree
isVarImportTests = testGroup "Semantic.NameEnv.isVarImport" $ map (\(name, varName, envs, out) ->
    testCase name $ isVarImport varName envs @?= out) [
    ("0", ["a"], [env1], True),
    ("1", ["b", "c"], [env1, env2], True),
    ("2", ["x"], [env1], False),
    ("3", ["a"], [], False)]
    where
        pos1, pos2 :: Position
        pos1 = makePosition 1 1 1
        pos2 = makePosition 2 1 1

        env1 :: ImportEnv
        env1 = IEnv {
            file = "f1",
            iVars = Map.fromList [(["a"], [pos1])],
            iFuncs = Map.empty
        }

        env2 :: ImportEnv
        env2 = IEnv {
            file = "f2",
            iVars = Map.fromList [(["b", "c"], [pos2])],
            iFuncs = Map.empty
        }


isFuncDefineTests :: TestTree
isFuncDefineTests = testGroup "Semantic.NameEnv.isFuncDefine" $ map (\(name, funName, st, out) ->
    testCase name $ isFuncDefine funName st @?= out) [
    ("0", ["f"], stWithF, True),
    ("1", ["g"], stWithGOuter, True),
    ("2", ["h"], stWithF, False),
    ("3", ["f"], stEmpty, False)]
    where
        pos1, pos2 :: Position
        pos1 = makePosition 1 1 1
        pos2 = makePosition 2 1 1

        scopeTop :: Scope
        scopeTop = Scope { scopeId = 0, sVars = Map.empty, sFuncs = Map.fromList [(["f"], [pos1])] }

        scopeOuter :: Scope
        scopeOuter = Scope { scopeId = 1, sVars = Map.empty, sFuncs = Map.fromList [(["g"], [pos2])] }

        stWithF :: CheckState
        stWithF = CheckState 0 0 0 [] [scopeTop] []

        stWithGOuter :: CheckState
        stWithGOuter = CheckState 0 0 0 [] [scopeTop, scopeOuter] []

        stEmpty :: CheckState
        stEmpty = CheckState 0 0 0 [] [] []


isFunImportTests :: TestTree
isFunImportTests = testGroup "Semantic.NameEnv.isFunImport" $ map (\(name, funName, envs, out) ->
    testCase name $ isFunImport funName envs @?= out) [
    ("0", ["f"], [env1], True),
    ("1", ["g", "h"], [env1, env2], True),
    ("2", ["x"], [env1], False),
    ("3", ["f"], [], False)]
    where
        pos1, pos2 :: Position
        pos1 = makePosition 1 1 1
        pos2 = makePosition 2 1 1

        env1 :: ImportEnv
        env1 = IEnv {
            file = "f1",
            iVars = Map.empty,
            iFuncs = Map.fromList [(["f"], [pos1])]
        }

        env2 :: ImportEnv
        env2 = IEnv {
            file = "f2",
            iVars = Map.empty,
            iFuncs = Map.fromList [(["g", "h"], [pos2])]
        }


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
    ("0", CheckState 0 0 0 [] [] [], 0),
    ("1", CheckState 1 0 0 [] [] [], 1),
    ("2", CheckState 2 10 0 [] [] [], 2),
    ("3", CheckState 3 42 0 [] [] [], 3)]


tests :: TestTree
tests = testGroup "Semantic.NameEnv" [
    forbiddenForTests, defineLocalVarTests, defineFuncTests,
    isVarDefineTests, isVarImportTests, isFuncDefineTests, isFunImportTests,
    getPackageNameTests, getStateDepthTests]
