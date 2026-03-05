module Semantic.ReturnCheckTest where

import Test.Tasty
import Test.Tasty.HUnit
import Util.Type (Position, makePosition)
import Parse.SyntaxTree (Block(..), Class(..), Command(..), Expression(..), Statement(..), SwitchCase(..))
import Semantic.ReturnCheck

import qualified Lex.Token as Lex
import qualified Util.Exception as UE


pos1 :: Position
pos1 = makePosition 1 1 1

pos2 :: Position
pos2 = makePosition 1 2 1

pos3 :: Position
pos3 = makePosition 1 3 1

pos4 :: Position
pos4 = makePosition 1 4 1


tok :: String -> Position -> Lex.Token
tok = Lex.Ident


tokNum :: String -> Position -> Lex.Token
tokNum = Lex.NumberConst


tokInt, tokVoid, tokRet, tokIf, tokCase, tokDefault, tokSwitch :: Lex.Token
tokInt = tok "int" pos1
tokVoid = tok "void" pos1
tokRet = tok "return" pos1
tokIf = tok "if" pos1
tokCase = tok "case" pos1
tokDefault = tok "default" pos1
tokSwitch = tok "switch" pos1


intExpr :: Expression
intExpr = IntConst "1" (tokNum "1" pos1)


varExpr :: String -> Position -> Expression
varExpr name p = Variable name (tok name p)


retStmt :: Statement
retStmt = Command (Return (Just intExpr)) tokRet


retBlock :: Block
retBlock = Multiple [retStmt]


noRetBlock :: Block
noRetBlock = Multiple [Expr intExpr]


mkFun :: Class -> [Lex.Token] -> Expression -> Block -> Statement
mkFun retT retToks name body = Function (retT, retToks) name [] body


missingMsg :: Class -> Expression -> [(Class, String, [Lex.Token])] -> Maybe [(Class, [Lex.Token])] -> String
missingMsg retT name params mt = UE.missingReturnMsg (funcSig retT name params mt)


firstWhy :: [UE.ErrorKind] -> Maybe String
firstWhy errors = case errors of
    (UE.Syntax (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Parsing (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Lexer (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    _ -> Nothing


assertErrs :: Maybe String -> [UE.ErrorKind] -> Assertion
assertErrs Nothing errs = errs @?= []
assertErrs (Just msg) errs = case firstWhy errs of
    Just whyMsg -> whyMsg @?= msg
    Nothing -> assertFailure $ "unexpected errors: " ++ show errs


returnCheckProgTests :: TestTree
returnCheckProgTests = testGroup "Semantic.ReturnCheck.returnCheckProg" $ map mkCase [
    ("0", ([], [mkFun Void [tokVoid] (varExpr "f" pos1) noRetBlock]), Nothing),
    ("1", ([], [mkFun Int32T [tokInt] (varExpr "g" pos2) retBlock]), Nothing),
    ("2", ([], [mkFun Int32T [tokInt] (varExpr "h" pos3) noRetBlock]),
        Just (missingMsg Int32T (varExpr "h" pos3) [] Nothing)),
    ("3", ([], [mkFun Int32T [] (varExpr "k" pos4) (Multiple [If intExpr (Just retBlock) (Just retBlock) (tokIf, Nothing)])]), Nothing)]
    where
        mkCase (name, prog, errMsg) = testCase name $ do
            let res = returnCheckProg "stdin" prog
            case res of
                Right () -> assertErrs errMsg []
                Left errs -> assertErrs errMsg errs


checkStmtTests :: TestTree
checkStmtTests = testGroup "Semantic.ReturnCheck.checkStmt" $ map mkCase [
    ("0", mkFun Int32T [tokInt] (varExpr "f" pos1) noRetBlock, Just (missingMsg Int32T (varExpr "f" pos1) [] Nothing)),
    ("1", mkFun Void [tokVoid] (varExpr "g" pos2) noRetBlock, Nothing),
    ("2", BlockStmt (Multiple [mkFun Int32T [tokInt] (varExpr "h" pos3) noRetBlock]),
        Just (missingMsg Int32T (varExpr "h" pos3) [] Nothing)),
    ("3", If intExpr (Just (Multiple [mkFun Int32T [tokInt] (varExpr "k" pos4) noRetBlock])) Nothing (tokIf, Nothing),
        Just (missingMsg Int32T (varExpr "k" pos4) [] Nothing))
    ]
    where
        mkCase (name, stmt, errMsg) = testCase name $
            assertErrs errMsg (checkStmt "stdin" stmt)


checkBlockTests :: TestTree
checkBlockTests = testGroup "Semantic.ReturnCheck.checkBlock" $ map mkCase [
    ("0", Multiple [], Nothing),
    ("1", Multiple [mkFun Int32T [tokInt] (varExpr "f" pos1) noRetBlock],
        Just (missingMsg Int32T (varExpr "f" pos1) [] Nothing)),
    ("2", Multiple [BlockStmt (Multiple [mkFun Int32T [tokInt] (varExpr "g" pos2) noRetBlock])],
        Just (missingMsg Int32T (varExpr "g" pos2) [] Nothing)),
    ("3", Multiple [If intExpr Nothing (Just (Multiple [mkFun Int32T [tokInt] (varExpr "h" pos3) noRetBlock])) (tokIf, Nothing)],
        Just (missingMsg Int32T (varExpr "h" pos3) [] Nothing))
    ]
    where
        mkCase (name, blk, errMsg) = testCase name $
            assertErrs errMsg (checkBlock "stdin" blk)


checkSwitchCaseTests :: TestTree
checkSwitchCaseTests = testGroup "Semantic.ReturnCheck.checkSwitchCase" $ map mkCase [
    ("0", Case intExpr Nothing tokCase, Nothing),
    ("1", Case intExpr (Just (Multiple [mkFun Int32T [tokInt] (varExpr "f" pos1) noRetBlock])) tokCase,
        Just (missingMsg Int32T (varExpr "f" pos1) [] Nothing)),
    ("2", Default (Multiple []) tokDefault, Nothing),
    ("3", Default (Multiple [mkFun Int32T [tokInt] (varExpr "g" pos2) noRetBlock]) tokDefault,
        Just (missingMsg Int32T (varExpr "g" pos2) [] Nothing))
    ]
    where
        mkCase (name, sc, errMsg) = testCase name $
            assertErrs errMsg (checkSwitchCase "stdin" sc)


checkFuncTests :: TestTree
checkFuncTests = testGroup "Semantic.ReturnCheck.checkFunc" $ map mkCase [
    ("0", Void, [tokVoid], varExpr "f" pos1, [], Nothing, noRetBlock, Nothing),
    ("1", Int32T, [tokInt], varExpr "g" pos2, [], Nothing, retBlock, Nothing),
    ("2", Int32T, [tokInt], varExpr "h" pos3, [], Nothing, noRetBlock,
        Just (missingMsg Int32T (varExpr "h" pos3) [] Nothing)),
    ("3", Int32T, [tokInt], varExpr "m" pos4, [(Int32T, "x", [tok "x" pos4])],
        Just [(Class ["T"] [], [tok "T" pos4])], noRetBlock,
        Just (missingMsg Int32T (varExpr "m" pos4) [(Int32T, "x", [tok "x" pos4])] (Just [(Class ["T"] [], [tok "T" pos4])])))
    ]
    where
        mkCase (name, retT, retToks, fname, params, mt, body, errMsg) = testCase name $
            assertErrs errMsg (checkFunc "stdin" retT retToks fname params mt body)


funcPosTests :: TestTree
funcPosTests = testGroup "Semantic.ReturnCheck.funcPos" $ map mkCase [
    ("0", [tokInt], varExpr "f" pos1, [pos1]),
    ("1", [], varExpr "g" pos2, [pos2]),
    ("2", [], Qualified ["A", "B"] [tok "A" pos3, tok "B" pos4], [pos3]),
    ("3", [], Error [] "err", [])
    ]
    where
        mkCase (name, toks, expr, expected) = testCase name $
            funcPos toks expr @?= expected


funcSigTests :: TestTree
funcSigTests = testGroup "Semantic.ReturnCheck.funcSig" $ map mkCase [
    ("0", Int32T, varExpr "f" pos1, [(Int32T, "x", [])], Nothing, "int f(int x)"),
    ("1", Void, Qualified ["A", "B"] [tok "A" pos2, tok "B" pos3], [], Nothing, "void A.B()"),
    ("2", Int32T, varExpr "g" pos2, [(Class ["T"] [], "t", [])], Just [(Class ["T"] [], [])], "int g<_T>(_T t)"),
    ("3", Float64T, varExpr "h" pos3, [(Int32T, "x", []), (Bool, "y", [])], Nothing, "double h(int x, bool y)")
    ]
    where
        mkCase (name, retT, fname, params, mt, expected) = testCase name $
            funcSig retT fname params mt @?= expected


funcNameTests :: TestTree
funcNameTests = testGroup "Semantic.ReturnCheck.funcName" $ map mkCase [
    ("0", varExpr "f" pos1, "f"),
    ("1", Qualified ["A", "B"] [tok "A" pos2, tok "B" pos3], "A.B"),
    ("2", intExpr, "<anonymous>"),
    ("3", Call (varExpr "g" pos2) [], "<anonymous>")
    ]
    where
        mkCase (name, expr, expected) = testCase name $
            funcName expr @?= expected


blockReturnsTests :: TestTree
blockReturnsTests = testGroup "Semantic.ReturnCheck.blockReturns" $ map mkCase [
    ("0", Multiple [], False),
    ("1", Multiple [retStmt], True),
    ("2", Multiple [If intExpr (Just retBlock) (Just retBlock) (tokIf, Nothing)], True),
    ("3", Multiple [If intExpr (Just retBlock) Nothing (tokIf, Nothing)], False)
    ]
    where
        mkCase (name, blk, expected) = testCase name $
            blockReturns blk @?= expected


stmtsReturnTests :: TestTree
stmtsReturnTests = testGroup "Semantic.ReturnCheck.stmtsReturn" $ map mkCase [
    ("0", [], False),
    ("1", [retStmt], True),
    ("2", [Command Break tokRet, retStmt], False),
    ("3", [BlockStmt retBlock, Expr intExpr], True)
    ]
    where
        mkCase (name, stmts, expected) = testCase name $
            stmtsReturn stmts @?= expected


stmtReturnsTests :: TestTree
stmtReturnsTests = testGroup "Semantic.ReturnCheck.stmtReturns" $ map mkCase [
    ("0", BlockStmt retBlock, True),
    ("1", If intExpr (Just retBlock) (Just retBlock) (tokIf, Nothing), True),
    ("2", If intExpr (Just retBlock) Nothing (tokIf, Nothing), False),
    ("3", Switch intExpr [Case intExpr (Just retBlock) tokCase, Default retBlock tokDefault] tokSwitch, True)
    ]
    where
        mkCase (name, stmt, expected) = testCase name $
            stmtReturns stmt @?= expected


blockReturnsMaybeTests :: TestTree
blockReturnsMaybeTests = testGroup "Semantic.ReturnCheck.blockReturnsMaybe" $ map mkCase [
    ("0", Nothing, False),
    ("1", Just retBlock, True),
    ("2", Just (Multiple []), False),
    ("3", Just (Multiple [If intExpr (Just retBlock) Nothing (tokIf, Nothing)]), False)
    ]
    where
        mkCase (name, blk, expected) = testCase name $
            blockReturnsMaybe blk @?= expected


switchReturnsTests :: TestTree
switchReturnsTests = testGroup "Semantic.ReturnCheck.switchReturns" $ map mkCase [
    ("0", [], False),
    ("1", [Case intExpr (Just retBlock) tokCase, Default retBlock tokDefault], True),
    ("2", [Case intExpr (Just retBlock) tokCase], False),
    ("3", [Case intExpr (Just (Multiple [])) tokCase, Default retBlock tokDefault], False)
    ]
    where
        mkCase (name, cases, expected) = testCase name $
            switchReturns cases @?= expected


caseReturnsTests :: TestTree
caseReturnsTests = testGroup "Semantic.ReturnCheck.caseReturns" $ map mkCase [
    ("0", Case intExpr Nothing tokCase, False),
    ("1", Case intExpr (Just retBlock) tokCase, True),
    ("2", Default retBlock tokDefault, True),
    ("3", Default (Multiple []) tokDefault, False)
    ]
    where
        mkCase (name, sc, expected) = testCase name $
            caseReturns sc @?= expected


isDefaultCaseTests :: TestTree
isDefaultCaseTests = testGroup "Semantic.ReturnCheck.isDefaultCase" $ map mkCase [
    ("0", Default retBlock tokDefault, True),
    ("1", Case intExpr Nothing tokCase, False),
    ("2", Default (Multiple []) tokDefault, True),
    ("3", Case intExpr (Just retBlock) tokCase, False)
    ]
    where
        mkCase (name, sc, expected) = testCase name $
            isDefaultCase sc @?= expected


tests :: TestTree
tests = testGroup "Semantic.ReturnCheck" [
    returnCheckProgTests, checkStmtTests, checkBlockTests, checkSwitchCaseTests,
    checkFuncTests, funcPosTests, funcSigTests, funcNameTests,
    blockReturnsTests, stmtsReturnTests, stmtReturnsTests, blockReturnsMaybeTests,
    switchReturnsTests, caseReturnsTests, isDefaultCaseTests
    ]
