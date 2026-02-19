{-# LANGUAGE TupleSections #-}

module Semantic.ContextCheckTest where

import Control.Monad.State.Strict (runState, execState)
import Test.Tasty
import Test.Tasty.HUnit
import Util.Type (Position, makePosition)
import Parse.ParseExpr (replLexparseExpr)
import Parse.ParseStmt (replLexparseStmt)
import Parse.ParseProgm (replLexparseProgm)
import Semantic.ContextCheck
import Semantic.NameEnv (CheckState(..), Scope(..), CtrlState(..), ImportEnv(..), QName)

import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


tp :: Int -> Lex.Token
tp i = Lex.Error ("test" ++ show i) pos1

pos1, pos2, pos3, pos4 :: Position
pos1 = makePosition 1 1 1
pos2 = makePosition 2 1 1
pos3 = makePosition 3 1 1
pos4 = makePosition 4 1 1

emptyScope :: Scope
emptyScope = Scope { scopeId = 0, sVars = Map.empty, sFuncs = Map.empty }

stEmpty :: CheckState
stEmpty = CheckState 0 0 0 [] [emptyScope] []

stFuncNoCtrl :: CheckState
stFuncNoCtrl = CheckState 1 2 3 [InFunction] [emptyScope] []

errE1, errE2, errE3, errE4 :: UE.ErrorKind
errE1 = mkSyntaxErr pos1 "e1"
errE2 = mkSyntaxErr pos2 "e2"
errE3 = mkSyntaxErr pos3 "e3"
errE4 = mkSyntaxErr pos4 "e4"

ctxNoErr0, ctxNoErr1, ctxNoErr2, ctxNoErr3 :: Ctx
ctxNoErr0 = mkCtx stEmpty []
ctxNoErr1 = ctxNoErr0
ctxNoErr2 = mkCtx stFuncNoCtrl []
ctxNoErr3 = ctxNoErr2

ctxErr0, ctxErr1, ctxErr2, ctxErr3 :: Ctx
ctxErr0 = mkCtx stEmpty []
ctxErr1 = mkCtx stEmpty [errE1]
ctxErr2 = mkCtx stFuncNoCtrl [errE1, errE2]
ctxErr3 = mkCtx stFuncNoCtrl []

noExtra :: Ctx -> Assertion
noExtra _ = pure ()

mkCtx :: CheckState -> [UE.ErrorKind] -> Ctx
mkCtx state errors = Ctx { st = state, errs = errors, varUses = Map.empty }

mkSyntaxErr :: Position -> String -> UE.ErrorKind
mkSyntaxErr pos msg = UE.Syntax $ UE.makeError "stdin" [pos] msg

assertErrs :: Maybe String -> Ctx -> Assertion
assertErrs Nothing ctx = errs ctx @?= []
assertErrs (Just msg) ctx = case errs ctx of
    [UE.Syntax (UE.BasicError { UE.why = whyMsg })] -> whyMsg @?= msg
    other -> assertFailure $ "unexpected errors: " ++ show other

assertState :: CheckState -> Ctx -> Assertion
assertState expected ctx = st ctx @?= expected

assertVarDefined :: String -> Ctx -> Assertion
assertVarDefined name ctx =
    Map.member name (sVars (head (scope (st ctx)))) @?= True

assertFuncDefined :: String -> Ctx -> Assertion
assertFuncDefined name ctx =
    Map.member [name] (sFuncs (head (scope (st ctx)))) @?= True

parseExprOrFail :: String -> IO AST.Expression
parseExprOrFail src = case replLexparseExpr src of
    Left errors -> assertFailure ("parse failed: " ++ show errors)
    Right expr -> pure expr

parseStmtOrFail :: String -> IO AST.Statement
parseStmtOrFail src = case replLexparseStmt src of
    Left errors -> assertFailure ("parse failed: " ++ show errors)
    Right stmt -> pure stmt

checkProgmFromSrc :: String -> Either [UE.ErrorKind] CheckState
checkProgmFromSrc src = case replLexparseProgm src of
    Left errors -> Left errors
    Right prog -> checkProgm "stdin" prog []

assertCheckProgm :: Either [UE.ErrorKind] CheckState -> Maybe String -> Assertion
assertCheckProgm res expected = case expected of
    Nothing -> case res of
        Right _ -> pure ()
        Left errors -> assertFailure $ "unexpected errors: " ++ show errors
    Just msg -> case res of
        Left errors -> case firstWhy errors of
            Just whyMsg -> whyMsg @?= msg
            Nothing -> assertFailure $ "unexpected errors: " ++ show errors
        Right _ -> assertFailure "expected errors but got success"
    where
        firstWhy :: [UE.ErrorKind] -> Maybe String
        firstWhy errors = case errors of
            (UE.Syntax (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
            (UE.Parsing (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
            (UE.Lexer (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
            _ -> Nothing

importVars :: [QName] -> ImportEnv
importVars names = IEnv {
    file = "stdin",
    iVars = Map.fromList $ map (, [pos1]) names,
    iFuncs = Map.empty
}

stWithVars :: [String] -> CheckState
stWithVars names = stEmpty {
    scope = [emptyScope { sVars = Map.fromList (map (, (0, pos1)) names) }]
}

stWithVarsFuncs :: [String] -> [String] -> CheckState
stWithVarsFuncs varNames funNames = stEmpty {
    scope = [emptyScope {
        sVars = Map.fromList (map (, (0, pos1)) varNames),
        sFuncs = Map.fromList (map (\n -> ([n], [pos1])) funNames)
    }]
}

stInBlock :: CheckState
stInBlock = CheckState 0 0 0 [InBlock] [emptyScope] []

stInBlockWithSum :: CheckState
stInBlockWithSum = stInBlock {
    scope = [emptyScope { sVars = Map.insert "sum" (0, pos1) Map.empty }]
}

stPut2, stPut3 :: CheckState
stPut2 = CheckState 2 5 6 [] [emptyScope] []
stPut3 = CheckState 0 0 1 [] [emptyScope { scopeId = 9 }] []


hasAssignTests :: TestTree
hasAssignTests = testGroup "Semantic.ContextCheck.hasAssign" $
    map (\(name, expr, out) -> testCase name $ hasAssign expr @?= out) [
    
    ("0", AST.IntConst "1" (tp 0), False),
    ("1", AST.Variable "a" (tp 1), False),
    ("2", AST.Qualified ["a","b"] [tp 2, tp 3], False),
    ("3", AST.Binary AST.Add (AST.IntConst "1" (tp 4)) (AST.IntConst "2" (tp 5)) (tp 6), False),
    ("4", AST.Binary AST.Assign (AST.Variable "a" (tp 7)) (AST.IntConst "10" (tp 8)) (tp 9), True),
    ("5", AST.Unary AST.Sub
            (AST.Binary AST.Assign (AST.Variable "a" (tp 10)) (AST.IntConst "1" (tp 11)) (tp 12))
            (tp 13),
        True),
        
    ("6", AST.Call (AST.Variable "f" (tp 14)) [
            AST.IntConst "1" (tp 15),
            AST.Binary AST.Assign (AST.Variable "x" (tp 16)) (AST.IntConst "2" (tp 17)) (tp 18)],
        True),
        
    ("7", AST.Binary AST.Add
            (AST.Binary AST.Assign (AST.Variable "a" (tp 19)) (AST.IntConst "1" (tp 20)) (tp 21))
            (AST.Binary AST.Assign (AST.Variable "b" (tp 22)) (AST.IntConst "2" (tp 23)) (tp 24))
            (tp 25),
        True)]



concatQTests :: TestTree
concatQTests = testGroup "Semantic.ContextCheck.concatQ" $ map (\(name, input, out) ->
    testCase name $ concatQ input @?= out) [
        ("0", [], ""),
        ("1", ["a"], "a"),
        ("2", ["a", "b"], "a.b"),
        ("3", ["a", "b", "c"], "a.b.c")]


addErrTests :: TestTree
addErrTests = testGroup "Semantic.ContextCheck.addErr" $ map (\(name, err, ctx, out) ->
    testCase name $ execState (addErr err) ctx @?= out) [
        ("0", errE1, ctxErr0, ctxErr0 { errs = [errE1] }),
        ("1", errE2, ctxErr1, ctxErr1 { errs = [errE2, errE1] }),
        ("2", errE3, ctxErr2, ctxErr2 { errs = [errE3, errE1, errE2] }),
        ("3", errE4, ctxErr3, ctxErr3 { errs = [errE4] })]


getStateTests :: TestTree
getStateTests = testGroup "Semantic.ContextCheck.getState" $ map (\(name, ctx, out) ->
    testCase name $ runState getState ctx @?= (out, ctx)) [
        ("0", ctxErr0, stEmpty),
        ("1", ctxErr1, stEmpty),
        ("2", ctxErr2, stFuncNoCtrl),
        ("3", ctxErr3, stFuncNoCtrl)]


putStateTests :: TestTree
putStateTests = testGroup "Semantic.ContextCheck.putState" $ map (\(name, newSt, ctx, out) ->
    testCase name $ execState (putState newSt) ctx @?= out) [
        ("0", stFuncNoCtrl, ctxErr0, ctxErr0 { st = stFuncNoCtrl }),
        ("1", stPut2, ctxErr1, ctxErr1 { st = stPut2 }),
        ("2", stEmpty, ctxErr2, ctxErr2 { st = stEmpty }),
        ("3", stPut3, ctxErr3, ctxErr3 { st = stPut3 })]


isFunctionTests :: TestTree
isFunctionTests = testGroup "Semantic.ContextCheck.isFunction" $ map (\(name, stmt, out) ->
    testCase name $ isFunction stmt @?= out) [
        ("0", stmtFun, True),
        ("1", stmtExpr, False),
        ("2", stmtCmd, False),
        ("3", stmtBlock, False)]
    where
        tokF, tokNum, tokCmd :: Lex.Token
        tokF = Lex.Ident "f" pos1
        tokNum = Lex.NumberConst "1" pos2
        tokCmd = Lex.Ident "break" pos3

        stmtFun, stmtExpr, stmtCmd, stmtBlock :: AST.Statement
        stmtFun = AST.Function (AST.Int32T, []) (AST.Variable "f" tokF) [] (AST.Multiple [])
        stmtExpr = AST.Expr (AST.IntConst "1" tokNum)
        stmtCmd = AST.Command AST.Break tokCmd
        stmtBlock = AST.BlockStmt (AST.Multiple [])


withCtrlTests :: TestTree
withCtrlTests = testGroup "Semantic.ContextCheck.withCtrl" $ map (\(name, ctrl, ctx) ->
    testCase name $ do
        let (res, finalCtx) = runState (withCtrl ctrl getState) ctx
        ctrlStack res @?= ctrl : ctrlStack (st ctx)
        ctrlStack (st finalCtx) @?= ctrlStack (st ctx)) [
        ("0", InLoop, ctxNoErr0),
        ("1", InIf, ctxNoErr2),
        ("2", InSwitch, ctxNoErr1),
        ("3", InFunction, ctxNoErr3)]


withScopeTests :: TestTree
withScopeTests = testGroup "Semantic.ContextCheck.withScope" $ map (\(name, ctx) ->
    testCase name $ do
        let stBefore = st ctx
            (res, finalCtx) = runState (withScope getState) ctx
            stAfter = st finalCtx
        depth res @?= depth stBefore + 1
        length (scope res) @?= length (scope stBefore) + 1
        scopeId (head (scope res)) @?= scopeCounter stBefore
        scopeCounter res @?= scopeCounter stBefore + 1
        depth stAfter @?= depth stBefore
        length (scope stAfter) @?= length (scope stBefore)
        scopeCounter stAfter @?= scopeCounter stBefore + 1) [
        ("0", ctxNoErr0),
        ("1", ctxNoErr1),
        ("2", ctxNoErr2),
        ("3", ctxNoErr3)]


withCtrlScopeTests :: TestTree
withCtrlScopeTests = testGroup "Semantic.ContextCheck.withCtrlScope" $ map (\(name, ctrl, ctx) ->
    testCase name $ do
        let stBefore = st ctx
            (res, finalCtx) = runState (withCtrlScope ctrl getState) ctx
            stAfter = st finalCtx
        ctrlStack res @?= ctrl : ctrlStack stBefore
        depth res @?= depth stBefore + 1
        length (scope res) @?= length (scope stBefore) + 1
        scopeCounter res @?= scopeCounter stBefore + 1
        ctrlStack stAfter @?= ctrlStack stBefore
        depth stAfter @?= depth stBefore
        length (scope stAfter) @?= length (scope stBefore)
        scopeCounter stAfter @?= scopeCounter stBefore + 1) [
        ("0", InLoop, ctxNoErr0),
        ("1", InIf, ctxNoErr2),
        ("2", InSwitch, ctxNoErr1),
        ("3", InFunction, ctxNoErr3)]


checkExprTests :: TestTree
checkExprTests = testGroup "Semantic.ContextCheck.checkExpr" $ map (\(name, src, state, envs, expected, extra) ->
    testCase name $ do
        expr <- parseExprOrFail src
        let ctx0 = Ctx { st = state, errs = [], varUses = Map.empty }
            (_, ctx1) = runState (checkExpr "stdin" [] envs expr) ctx0
        assertErrs expected ctx1
        extra ctx1) [
        ("0", "x + 1 * 2", stWithVars ["x"], [], Nothing, noExtra),
        ("1", "x + y + 1", stWithVars ["x"], [], Just (UE.undefinedIdentity "y"), noExtra),
        ("2", "a.b.c + x * (2 + 3)", stWithVars ["x"], [importVars [["a", "b", "c"]]], Nothing, noExtra),
        ("3", "x + y * (z + 2)", stWithVars ["x", "y"], [], Just (UE.undefinedIdentity "z"), noExtra),
        ("4", "x = x + 1", stWithVars ["x"], [], Nothing, assertVarDefined "x"),
        ("5", "a.b = x + 1", stWithVars ["x"], [], Just UE.assignErrorMsg, noExtra),
        ("6", "y = x + z * f(2)", stWithVarsFuncs ["x", "z"] ["f"], [], Nothing, assertVarDefined "y"),
        ("7", "g(x + 1, 2 * 3)", stWithVars ["x"], [], Just (UE.undefinedIdentity "g"), noExtra)]


isContinueValidTests :: TestTree
isContinueValidTests = testGroup "Semantic.ContextCheck.isContinueValid" $ map (\(name, ctrls, out) ->
    testCase name $ isContinueValid ctrls @?= out) [
        ("0", [InLoop], True),
        ("1", [InFunction, InLoop], True),
        ("2", [InSwitch, InCase], False),
        ("3", [], False)]


isBreakValidTests :: TestTree
isBreakValidTests = testGroup "Semantic.ContextCheck.isBreakValid" $ map (\(name, ctrls, out) ->
    testCase name $ isBreakValid ctrls @?= out) [
        ("0", [InLoop], True),
        ("1", [InCase], True),
        ("2", [InFunction], False),
        ("3", [], False)]


isReturnValidTests :: TestTree
isReturnValidTests = testGroup "Semantic.ContextCheck.isReturnValid" $ map (\(name, ctrls, out) ->
    testCase name $ isReturnValid ctrls @?= out) [
        ("0", [InFunction], True),
        ("1", [InFunction, InLoop], True),
        ("2", [InLoop, InCase], False),
        ("3", [], False)]


checkStmtTests :: TestTree
checkStmtTests = testGroup "Semantic.ContextCheck.checkStmt" $ map (\(name, stmtOrSrc, initSt) ->
    testCase name $ do
        stmt <- case stmtOrSrc of
            Left src -> parseStmtOrFail src
            Right stmt -> pure stmt
        let ctx0 = Ctx { st = initSt, errs = [], varUses = Map.empty }
            (_, ctx1) = runState (checkStmt "stdin" [] [] stmt) ctx0
        errs ctx1 @?= []) [
        ("0", Left $ unlines [
            "{",
            "    x = 1",
            "    y = 2",
            "}"
        ], stInBlock),
        ("1", Left $ unlines [
            "while true:",
            "    x = 1",
            "else:",
            "    y = 2"
        ], stInBlock),
        ("2", Left $ unlines [
            "if true:",
            "    x = 1",
            "else:",
            "    y = 2"
        ], stInBlock),
        ("3", Right doWhileStmt, stInBlock),
        ("4", Right forStmt, stInBlockWithSum)]
    where
        tokDo, tokWhile, tokElse, tokTrue, tokFor :: Lex.Token
        tokDo = Lex.Ident "do" pos1
        tokWhile = Lex.Ident "while" pos2
        tokElse = Lex.Ident "else" pos3
        tokTrue = Lex.Ident "true" pos4
        tokFor = Lex.Ident "for" pos1

        tokX, tokY, tokI, tokSum, tokAssign, tokPlusAssign, tokInc, tokLt, tokNum0, tokNum1, tokNum2, tokNum10 :: Lex.Token
        tokX = Lex.Ident "x" pos1
        tokY = Lex.Ident "y" pos2
        tokI = Lex.Ident "i" pos1
        tokSum = Lex.Ident "sum" pos2
        tokAssign = Lex.Symbol Lex.Assign pos3
        tokPlusAssign = Lex.Symbol Lex.PlusAssign pos3
        tokInc = Lex.Symbol Lex.PlusPlus pos4
        tokLt = Lex.Symbol Lex.LessThan pos2
        tokNum0 = Lex.NumberConst "0" pos1
        tokNum1 = Lex.NumberConst "1" pos3
        tokNum2 = Lex.NumberConst "2" pos4
        tokNum10 = Lex.NumberConst "10" pos2

        assignX :: AST.Statement
        assignX = AST.Expr (AST.Binary AST.Assign (AST.Variable "x" tokX) (AST.IntConst "1" tokNum1) tokAssign)

        assignY :: AST.Statement
        assignY = AST.Expr (AST.Binary AST.Assign (AST.Variable "y" tokY) (AST.IntConst "2" tokNum2) tokAssign)

        doWhileStmt :: AST.Statement
        doWhileStmt =
            AST.DoWhile
                (Just (AST.Multiple [assignX]))
                (AST.BoolConst True tokTrue)
                (Just (AST.Multiple [assignY]))
                (tokDo, tokWhile, Just tokElse)

        forStmt :: AST.Statement
        forStmt =
            AST.For
                ( Just (AST.Binary AST.Assign (AST.Variable "i" tokI) (AST.IntConst "0" tokNum0) tokAssign)
                , Just (AST.Binary AST.LessThan (AST.Variable "i" tokI) (AST.IntConst "10" tokNum10) tokLt)
                , Just (AST.Unary AST.SelfInc (AST.Variable "i" tokI) tokInc)
                )
                ( Just (AST.Multiple [
                    AST.Expr (AST.Binary AST.PlusAssign (AST.Variable "sum" tokSum) (AST.Variable "i" tokI) tokPlusAssign)
                ]))
                tokFor

checkSwitchCaseTests :: TestTree
checkSwitchCaseTests = testGroup "Semantic.ContextCheck.checkSwitchCase" $ map (\(name, sc, initSt, expected, extra) ->
    testCase name $ do
        let ctx0 = Ctx { st = initSt, errs = [], varUses = Map.empty }
            (_, ctx1) = runState (checkSwitchCase "stdin" [] [] sc) ctx0
        assertErrs expected ctx1
        extra ctx1) [
        ("0", scCaseX, stWithX, Nothing, assertState stWithX),
        ("1", scCaseY, stEmpty, Just (UE.undefinedIdentity "y"), noExtra),
        ("2", scCaseContinue, stEmpty, Just UE.continueCtrlErrorMsg, noExtra),
        ("3", scDefaultBreak, stEmpty, Nothing, noExtra)]
    where
        tokX, tokY, tokTrue, tokContinue, tokBreak, tokCase, tokDefault :: Lex.Token
        tokX = Lex.Ident "x" pos1
        tokY = Lex.Ident "y" pos1
        tokTrue = Lex.Ident "true" pos1
        tokContinue = Lex.Ident "continue" pos1
        tokBreak = Lex.Ident "break" pos1
        tokCase = Lex.Ident "case" pos1
        tokDefault = Lex.Ident "default" pos1

        exprX :: AST.Expression
        exprX = AST.Variable "x" tokX

        exprY :: AST.Expression
        exprY = AST.Variable "y" tokY

        exprTrue :: AST.Expression
        exprTrue = AST.BoolConst True tokTrue

        blockContinue :: AST.Block
        blockContinue = AST.Multiple [AST.Command AST.Continue tokContinue]

        blockBreak :: AST.Block
        blockBreak = AST.Multiple [AST.Command AST.Break tokBreak]

        scCaseX, scCaseY, scCaseContinue :: AST.SwitchCase
        scCaseX = AST.Case exprX Nothing tokCase
        scCaseY = AST.Case exprY Nothing tokCase
        scCaseContinue = AST.Case exprTrue (Just blockContinue) tokCase

        scDefaultBreak :: AST.SwitchCase
        scDefaultBreak = AST.Default blockBreak tokDefault

        stWithX :: CheckState
        stWithX = stEmpty {
            scope = [emptyScope { sVars = Map.fromList [("x", (0, pos1))] }]
        }


checkBlockTests :: TestTree
checkBlockTests = testGroup "Semantic.ContextCheck.checkBlock" $ map (\(name, block, initSt, expected, extra) ->
    testCase name $ do
        let ctx0 = Ctx { st = initSt, errs = [], varUses = Map.empty }
            (_, ctx1) = runState (checkBlock "stdin" [] [] block) ctx0
        assertErrs expected ctx1
        extra ctx1) [
        ("0", blockDefineUse, stEmpty, Nothing, assertVarDefined "x"),
        ("1", blockUndefinedVar, stEmpty, Just (UE.undefinedIdentity "y"), noExtra),
        ("2", blockBreak, stEmpty, Just UE.breakCtrlErrorMsg, noExtra),
        ("3", blockCallBeforeDef, stEmpty, Nothing, assertFuncDefined "f")]
    where
        tokX, tokY, tokF, tokAssign, tokPlus, tokNum1, tokBreak :: Lex.Token
        tokX = Lex.Ident "x" pos1
        tokY = Lex.Ident "y" pos1
        tokF = Lex.Ident "f" pos1
        tokAssign = Lex.Symbol Lex.Assign pos1
        tokPlus = Lex.Symbol Lex.Plus pos1
        tokNum1 = Lex.NumberConst "1" pos1
        tokBreak = Lex.Ident "break" pos1

        assignX :: AST.Statement
        assignX = AST.Expr (AST.Binary AST.Assign (AST.Variable "x" tokX) (AST.IntConst "1" tokNum1) tokAssign)

        useX :: AST.Statement
        useX = AST.Expr (AST.Binary AST.Add (AST.Variable "x" tokX) (AST.IntConst "1" tokNum1) tokPlus)

        useY :: AST.Statement
        useY = AST.Expr (AST.Binary AST.Add (AST.Variable "y" tokY) (AST.IntConst "1" tokNum1) tokPlus)

        callF :: AST.Statement
        callF = AST.Expr (AST.Call (AST.Variable "f" tokF) [])

        funDefF :: AST.Statement
        funDefF = AST.Function (AST.Int32T, []) (AST.Variable "f" tokF) [] (AST.Multiple [])

        blockDefineUse :: AST.Block
        blockDefineUse = AST.Multiple [assignX, useX]

        blockUndefinedVar :: AST.Block
        blockUndefinedVar = AST.Multiple [useY]

        blockBreak :: AST.Block
        blockBreak = AST.Multiple [AST.Command AST.Break tokBreak]

        blockCallBeforeDef :: AST.Block
        blockCallBeforeDef = AST.Multiple [callF, funDefF]


checkStmtsTests :: TestTree
checkStmtsTests = testGroup "Semantic.ContextCheck.checkStmts" $ map (\(name, stmts, initSt, expected, extra) ->
    testCase name $ do
        let ctx0 = Ctx { st = initSt, errs = [], varUses = Map.empty }
            (_, ctx1) = runState (checkStmts "stdin" [] [] stmts) ctx0
        assertErrs expected ctx1
        extra ctx1) [
        ("0", [], stEmpty, Nothing, assertState stEmpty),
        ("1", [callF, funDefF], stEmpty, Nothing, assertFuncDefined "f"),
        ("2", [callG], stEmpty, Just (UE.undefinedIdentity "g"), noExtra),
        ("3", [funDefQualified], stEmpty, Just UE.unsupportedErrorMsg, noExtra)]
    where
        tokF, tokG, tokA :: Lex.Token
        tokF = Lex.Ident "f" pos1
        tokG = Lex.Ident "g" pos1
        tokA = Lex.Ident "A" pos1

        callF :: AST.Statement
        callF = AST.Expr (AST.Call (AST.Variable "f" tokF) [])

        callG :: AST.Statement
        callG = AST.Expr (AST.Call (AST.Variable "g" tokG) [])

        funDefF :: AST.Statement
        funDefF = AST.Function (AST.Int32T, []) (AST.Variable "f" tokF) [] (AST.Multiple [])

        funDefQualified :: AST.Statement
        funDefQualified =
            AST.Function (AST.Int32T, []) (AST.Qualified ["A", "f"] [tokA, tokF]) [] (AST.Multiple [])


checkProgmTests :: TestTree
checkProgmTests = testGroup "Semantic.ContextCheck.checkProgm" (
    map mkCase [
        ("0", unlines [
            "int f() { }",
            "int g(int x) { }",
            "a = f()",
            "b = g(1)",
            "c = a + b",
            "d = c + 2"
        ], Nothing),
        ("1", unlines [
            "a = 1",
            "b = a + 2",
            "c = b + 3",
            "d = c + e"
        ], Just (UE.undefinedIdentity "e")),
        ("2", unlines [
            "a = f()",
            "b = a + 1",
            "c = b + 2",
            "d = c + g(1)"
        ], Just (UE.undefinedIdentity "f")),
        ("3", unlines [
            "int f() { }",
            "int g(int x) { }",
            "a = f() + g(1)",
            "b = a + c",
            "d = b + 1",
            "e = d + 2"
        ], Just (UE.undefinedIdentity "c")),
        ("4", unlines [
            "{",
            "    a = 1",
            "    b = 2;",
            "}",
            "c = 3",
            "d = 4"
        ], Just (UE.illegalStatementMsg "block" "class")),
        ("5", unlines [
            "int main() {",
            "    {",
            "        x = 1",
            "        y = x + 1;",
            "    }",
            "    z = 2",
            "    w = z + 1",
            "}",
            "p = 1",
            "q = p + 1",
            "r = q + 1"
        ], Nothing),
        ("6", unlines [
            "int main() {",
            "    {",
            "        x = 1;",
            "        y = x + 1;",
            "    }",
            "    z = x + 2",
            "    w = z + 1",
            "}",
            "p = 1",
            "q = p + 1",
            "r = q + 1"
        ], Just (UE.undefinedIdentity "x")),
        ("7", unlines [
            "int main() {",
            "    a = 1",
            "    {",
                "        b = a + 1",
                "        c = b + 1",
            "    }",
            "    d = a + 2",
            "    e = d + 1",
            "}",
            "p = 1",
            "q = p + 1",
            "r = q + 1"
        ], Nothing),
        ("8", unlines [
            "a = f()",
            "b = a + 1",
            "c = b + 1",
            "d = c + 1",
            "int f() { }"
        ], Nothing),
        ("9", unlines [
            "int f() { }",
            "a = f()",
            "b = g(1)",
            "c = b + 1",
            "d = c + 1"
        ], Just (UE.undefinedIdentity "g")),
        ("10", unlines [
            "int outer() {",
            "    int inner() { }",
            "    a = inner()",
            "    b = a + 1",
            "    c = b + 1",
            "}",
            "d = 1",
            "e = d + 1",
            "f = e + 1"
        ], Nothing),
        ("11", unlines [
            "int outer() {",
            "    int inner() { }",
            "    a = inner()",
            "    b = a + 1",
            "    c = b + 1",
            "}",
            "x = inner()",
            "y = x + 1",
            "z = y + 1"
        ], Just (UE.undefinedIdentity "inner")),
        ("12", unlines [
            "int main() {",
            "    a = 0",
            "    if true:",
            "        a = 1",
            "    else:",
            "        a = 2",
            "    b = a + 1",
            "    c = b + 1",
            "}",
            "d = 1",
            "e = d + 1",
            "f = e + 1"
        ], Nothing),
        ("13", unlines [
            "int main() {",
            "    if true:",
            "        a = 1",
            "    else:",
            "        a = 2",
            "    b = a + 1",
            "    c = b + 1",
            "}",
            "d = 1",
            "e = d + 1",
            "f = e + 1"
        ], Just (UE.undefinedIdentity "a")),
        ("14", unlines [
            "int main() {",
            "    i = 0",
            "    while true:",
            "        i = i + 1",
            "    else:",
            "        i = i + 2",
            "    j = i + 1",
            "    k = j + 1",
            "}",
            "a = 1",
            "b = a + 1",
            "c = b + 1"
        ], Nothing),
        ("15", unlines [
            "int main() {",
            "    while true:",
            "        i = 1",
            "    else:",
            "        i = 2",
            "    j = i + 1",
            "    k = j + 1",
            "}",
            "a = 1",
            "b = a + 1",
            "c = b + 1"
        ], Just (UE.undefinedIdentity "i"))] ++ [
        
        testCase "16" $ do
        eInit <- parseExprOrFail "i = 0"
        eInc <- parseExprOrFail "i = i + 1"
        eCond <- parseExprOrFail "i < 10"
        eElse <- parseExprOrFail "i = i + 2"
        eJ <- parseExprOrFail "j = i + 1"
        eK <- parseExprOrFail "k = j + 1"
        let tokDo = Lex.Ident "do" pos1
            tokWhile = Lex.Ident "while" pos2
            tokElse = Lex.Ident "else" pos3
            tokMain = Lex.Ident "main" pos1
            doStmt = AST.DoWhile
                (Just (AST.Multiple [AST.Expr eInc]))
                eCond
                (Just (AST.Multiple [AST.Expr eElse]))
                (tokDo, tokWhile, Just tokElse)
            body = AST.Multiple [AST.Expr eInit, doStmt, AST.Expr eJ, AST.Expr eK]
            fun = AST.Function (AST.Int32T, []) (AST.Variable "main" tokMain) [] body
            prog = ([], [fun])
        assertCheckProgm (checkProgm "stdin" prog []) Nothing,
        
        testCase "17" $ do
        eInit <- parseExprOrFail "i = 0"
        eCond <- parseExprOrFail "i < 10"
        eElse <- parseExprOrFail "i = i + 2"
        eA <- parseExprOrFail "a = 1"
        eB <- parseExprOrFail "b = a + 1"
        eC <- parseExprOrFail "c = b + 1"
        let tokDo = Lex.Ident "do" pos1
            tokWhile = Lex.Ident "while" pos2
            tokElse = Lex.Ident "else" pos3
            tokMain = Lex.Ident "main" pos1
            doStmt = AST.DoWhile
                (Just (AST.Multiple [AST.Expr eInit]))
                eCond
                (Just (AST.Multiple [AST.Expr eElse]))
                (tokDo, tokWhile, Just tokElse)
            body = AST.Multiple [doStmt, AST.Expr eA, AST.Expr eB, AST.Expr eC]
            fun = AST.Function (AST.Int32T, []) (AST.Variable "main" tokMain) [] body
            prog = ([], [fun])
        assertCheckProgm (checkProgm "stdin" prog []) (Just (UE.undefinedIdentity "i"))
    ])
    where
        mkCase (name, src, expected) =
            testCase name $ assertCheckProgm (checkProgmFromSrc src) expected


forScopeTests :: TestTree
forScopeTests = testGroup "Semantic.ContextCheck.forScope" [
    testCase "0_new_var_scoped" $ do
        let ctx0 = Ctx { st = stInBlock, errs = [], varUses = Map.empty }
            (_, ctx1) = runState (checkStmt "stdin" [] [] forStmt) ctx0
            stAfter = st ctx1
        errs ctx1 @?= []
        Map.member "i" (sVars (head (scope stAfter))) @?= False
        varCounter stAfter @?= varCounter stInBlock + 1,
    testCase "1_reuse_outer_var" $ do
        let st0 = stInBlock { varCounter = 1, scope = [emptyScope { sVars = Map.insert "i" (0, pos1) Map.empty }] }
            ctx0 = Ctx { st = st0, errs = [], varUses = Map.empty }
            (_, ctx1) = runState (checkStmt "stdin" [] [] forStmt) ctx0
            stAfter = st ctx1
        errs ctx1 @?= []
        Map.lookup "i" (sVars (head (scope stAfter))) @?= Just (0, pos1)
        varCounter stAfter @?= varCounter st0]
    where
        tokFor, tokI, tokAssign, tokInc, tokLt, tokNum0, tokNum10 :: Lex.Token
        tokFor = Lex.Ident "for" pos1
        tokI = Lex.Ident "i" pos1
        tokAssign = Lex.Symbol Lex.Assign pos1
        tokInc = Lex.Symbol Lex.PlusPlus pos1
        tokLt = Lex.Symbol Lex.LessThan pos1
        tokNum0 = Lex.NumberConst "0" pos1
        tokNum10 = Lex.NumberConst "10" pos1

        forStmt :: AST.Statement
        forStmt =
            AST.For
                ( Just (AST.Binary AST.Assign (AST.Variable "i" tokI) (AST.IntConst "0" tokNum0) tokAssign)
                , Just (AST.Binary AST.LessThan (AST.Variable "i" tokI) (AST.IntConst "10" tokNum10) tokLt)
                , Just (AST.Unary AST.SelfInc (AST.Variable "i" tokI) tokInc)
                )
                ( Just (AST.Multiple []))
                tokFor

tests :: TestTree
tests = testGroup "Semantic.ContextCheck" [
    hasAssignTests,
    concatQTests, addErrTests, getStateTests, putStateTests, isFunctionTests,
    withCtrlTests, withScopeTests, withCtrlScopeTests,
    isContinueValidTests, isBreakValidTests, isReturnValidTests,
    
    checkExprTests, checkStmtTests, checkSwitchCaseTests, checkBlockTests, checkStmtsTests, checkProgmTests,
    forScopeTests]
