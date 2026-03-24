{-# LANGUAGE PatternSynonyms #-}

module IR.LowingTest where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (encode)
import Data.List (find, isInfixOf)
import IR.Lowing (codeToIRSingleWithRoot)
import IR.TAC (IRAtom(..), IRBlock (..), IRClass (..), IRFunction (..), IRInstr (..), IRProgm (..))
import Lowing.JVMLowing (jvmProgmLowing)
import Lowing.JVMJson (jProgmToJSON)
import Parse.ParserBasic (DeclFlag (..))
import Parse.SyntaxTree (Class (..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


findAttr ::
    String ->
    [((a, [DeclFlag]), Class, String, b)] ->
    Maybe ((a, [DeclFlag]), Class, String, b)
findAttr name = find (\(_, _, attrName, _) -> attrName == name)


firstWhy :: [UE.ErrorKind] -> Maybe String
firstWhy errors = case errors of
    (UE.Syntax (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Parsing (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Lexer (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    _ -> Nothing


topLevelValFinalTests :: TestTree
topLevelValFinalTests = testGroup "IR.Lowing.topLevelValFinal" $ map (uncurry testCase) [
    ("0", do
        let src = unlines [
                "val pi = 3.14",
                "var n = 1"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ attrs _ _ _ _], _) -> do
                case findAttr "pi" attrs of
                    Nothing -> assertFailure "missing field: pi"
                    Just ((_, flags), cls, _, _) -> do
                        cls @?= Float64T
                        assertBool "pi should be static" (Static `elem` flags)
                        assertBool "pi should be final" (Final `elem` flags)
                case findAttr "n" attrs of
                    Nothing -> assertFailure "missing field: n"
                    Just ((_, flags), cls, _, _) -> do
                        cls @?= Int32T
                        assertBool "n should be static" (Static `elem` flags)
                        assertBool "n should not be final" (Final `notElem` flags)
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ),

    ("1", do
        let src = unlines [
                "val x = 1",
                "x = 2"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> firstWhy errs @?= Just (UE.immutableVariableMsg "x")
            Right _ -> assertFailure "expected immutable-variable error"
    ),

    ("2", do
        let src = unlines [
                "int f() {",
                "    val x = 1",
                "    {",
                "        val x = 2",
                "    }",
                "    return x",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right _ -> pure ()
    ),

    ("3", do
        let src = unlines [
                "int f() {",
                "    val x = 1",
                "    val x = 2",
                "    return x",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> firstWhy errs @?= Just (UE.multipleVariableDefMsg "x")
            Right _ -> assertFailure "expected duplicate-variable error"
    )]

callArgCastTests :: TestTree
callArgCastTests = testGroup "IR.Lowing.callArgCast" [
    testCase "insert cast when calling double param with int arg" $ do
        let src = unlines [
                "double f(double x) {",
                "    return x",
                "}",
                "double g() {",
                "    return f(1)",
                "}"
                ]
            collectInstrs :: [IRBlock] -> [IRInstr]
            collectInstrs = concatMap (\(IRBlock (_, body)) -> body)
            hasIntToDoubleCast :: [IRInstr] -> Bool
            hasIntToDoubleCast = any isCast
                where
                    isCast instr = case instr of
                        ICast _ (Int32T, Float64T) _ -> True
                        _ -> False

        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ funs _], _) -> do
                case find (\(IRFunction _ name _ _ _ _) -> name == "g") funs of
                    Nothing -> assertFailure "missing function g"
                    Just (IRFunction _ _ _ _ blocks _) ->
                        assertBool "g should cast int argument to double before call"
                            (hasIntToDoubleCast (collectInstrs blocks))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]

ternaryControlFlowTests :: TestTree
ternaryControlFlowTests = testGroup "IR.Lowing.ternaryControlFlow" [
    testCase "return-ternary dispatch stays in block join" $ do
        let src = unlines [
                "int f(int x) {",
                "    return -1 if x < 0 else 0",
                "}"
                ]
            instrHasIfeq instr = case instr of
                Ifeq {} -> True
                _ -> False
            instrHasSetRet instr = case instr of
                SetIRet {} -> True
                _ -> False

        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "f" _ _ blocks _] _], _) -> do
                let blockBodies = [body | IRBlock (_, body) <- blocks]
                    topHasIfeq = False
                    topHasSetRet = False
                    blockHasIfeq = any (any instrHasIfeq) blockBodies
                    blockHasSetRet = any (any instrHasSetRet) blockBodies

                assertBool "top-level Ifeq should not appear for ternary return" (not topHasIfeq)
                assertBool "top-level SetIRet should not appear for ternary return" (not topHasSetRet)
                assertBool "a block should contain ternary branch Ifeq" blockHasIfeq
                assertBool "a block should contain SetIRet after ternary join" blockHasSetRet
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]


stringLiteralLoweringTests :: TestTree
stringLiteralLoweringTests = testGroup "IR.Lowing.stringLiteral" [
    testCase "lower val string without semantic/type errors" $ do
        let src = unlines [
                "int main() {",
                "    val a = \"100\"",
                "    return 0",
                "}"
                ]
            collectInstrs :: [IRBlock] -> [IRInstr]
            collectInstrs = concatMap (\(IRBlock (_, body)) -> body)
            hasStringAssign :: [IRInstr] -> Bool
            hasStringAssign = any isStringAssign
                where
                    isStringAssign instr = case instr of
                        IAssign _ (StringC "100") -> True
                        _ -> False

        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "main" _ _ blocks _] _], _) -> do
                assertBool "main should contain string literal assignment lowering"
                    (hasStringAssign (collectInstrs blocks))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ,
    testCase "jvm json contains reference ops for string local" $ do
        let src = unlines [
                "int main() {",
                "    val a = \"100\"",
                "    return 0",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (ir@(IRProgm _ _), _) -> do
                let classes = jvmProgmLowing ir
                    jsonText = BL.unpack (encode (jProgmToJSON 8 classes))
                assertBool "json should include string push op" ("\"op_name\":\"apush\"" `isInfixOf` jsonText)
                assertBool "json should include reference local store op" ("\"op_name\":\"astore\"" `isInfixOf` jsonText)
    ]


staticFieldReadLoweringTests :: TestTree
staticFieldReadLoweringTests = testGroup "IR.Lowing.staticFieldRead" [
    testCase "top-level static read in function lowers to getstatic" $ do
        let src = unlines [
                "int a = 10",
                "int main() {",
                "    return a",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (ir@(IRProgm _ _), _) -> do
                let classes = jvmProgmLowing ir
                    jsonText = BL.unpack (encode (jProgmToJSON 8 classes))
                assertBool "json should include getstatic for top-level field read" ("\"op_name\":\"getstatic\"" `isInfixOf` jsonText)
                assertBool "json should reference MainX.a in getstatic" ("\"attr_name\":[\"MainX\",\"a\"]" `isInfixOf` jsonText)
    ]

doubleCmpLoweringTests :: TestTree
doubleCmpLoweringTests = testGroup "IR.Lowing.doubleCmp" [
    testCase "double condition lowers to dcmp-based if op" $ do
        let src = unlines [
                "int main() {",
                "    val a = 10.0",
                "    val absA = a if a > 0 else (-a)",
                "    return 0",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (ir@(IRProgm _ _), _) -> do
                let classes = jvmProgmLowing ir
                    jsonText = BL.unpack (encode (jProgmToJSON 8 classes))
                assertBool "json should include if_dcmp* for double comparisons"
                    ("\"op_name\":\"if_dcmp" `isInfixOf` jsonText)
                assertBool "json should not include if_fcmp* for double comparisons"
                    (not ("\"op_name\":\"if_fcmp" `isInfixOf` jsonText))
    ]

float128JvmRejectTests :: TestTree
float128JvmRejectTests = testGroup "IR.Lowing.float128JvmReject" [
    testCase "jvm lowering rejects float128 (native only)" $ do
        let src = unlines [
                "float128 a = 1",
                "int main() {",
                "    return 0",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (ir@(IRProgm _ _), _) -> do
                res <- try (evaluate (length (jvmProgmLowing ir))) :: IO (Either SomeException Int)
                case res of
                    Left ex ->
                        assertBool "error should mention float128 is native-only"
                            ("float128 is native-only" `isInfixOf` show ex)
                    Right _ -> assertFailure "expected jvm lowering to reject float128"
    ]


incDecLoweringTests :: TestTree
incDecLoweringTests = testGroup "IR.Lowing.incDecLowering" [
    testCase "postfix ++ lowers with add" $ do
        let src = unlines [
                "int main() {",
                "    var a = 1",
                "    var b = a++",
                "    return b",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "main" _ _ blocks _] _], _) -> do
                let instrs = collectInstrs blocks
                assertBool "postfix ++ should generate add op" (any isAdd instrs)
                assertBool "postfix ++ should not generate sub op in this snippet" (not (any isSub instrs))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir),

    testCase "postfix -- lowers with sub" $ do
        let src = unlines [
                "int main() {",
                "    var a = 1",
                "    var b = a--",
                "    return b",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "main" _ _ blocks _] _], _) -> do
                let instrs = collectInstrs blocks
                assertBool "postfix -- should generate sub op" (any isSub instrs)
                assertBool "postfix -- should not generate add op in this snippet" (not (any isAdd instrs))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir),

    testCase "prefix ++ on var lowers with add" $ do
        let src = unlines [
                "int main() {",
                "    var a = 1",
                "    return ++a",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "main" _ _ blocks _] _], _) -> do
                assertBool "prefix ++ should generate add op" (any isAdd (collectInstrs blocks))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir),

    testCase "prefix -- on var lowers with sub" $ do
        let src = unlines [
                "int main() {",
                "    var a = 1",
                "    return --a",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "main" _ _ blocks _] _], _) -> do
                assertBool "prefix -- should generate sub op" (any isSub (collectInstrs blocks))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]
    where
        collectInstrs :: [IRBlock] -> [IRInstr]
        collectInstrs = concatMap (\(IRBlock (_, body)) -> body)

        isAdd :: IRInstr -> Bool
        isAdd instr = case instr of
            IBinary _ AST.Add _ _ -> True
            _ -> False

        isSub :: IRInstr -> Bool
        isSub instr = case instr of
            IBinary _ AST.Sub _ _ -> True
            _ -> False

assignmentRhsIsolationTests :: TestTree
assignmentRhsIsolationTests = testGroup "IR.Lowing.assignmentRhsIsolation" [
    testCase "rhs temporaries should not reuse lhs var versions" $ do
        let src = unlines [
                "int main() {",
                "    var x = 10.0",
                "    val eta = 0.1",
                "    x = x - eta * 2.0 * x",
                "    return 0",
                "}"
                ]
            isBuggySelfMul instr = case instr of
                IBinary _ AST.Mul (Var (n1, _, v1)) (Var (n2, _, v2)) ->
                    n1 == "x" && n2 == "x" && v1 == v2
                _ -> False
            collectInstrs :: [IRBlock] -> [IRInstr]
            collectInstrs = concatMap (\(IRBlock (_, body)) -> body)

        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "main" _ _ blocks _] _], _) ->
                assertBool "rhs lowering should not turn eta*2*x into x*x"
                    (not (any isBuggySelfMul (collectInstrs blocks)))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]


shortCircuitAssignPlacementTests :: TestTree
shortCircuitAssignPlacementTests = testGroup "IR.Lowing.shortCircuitAssignPlacement" [
    testCase "repeat body assignments after !&& should not be placed after unconditional jump" $ do
        let src = unlines [
                "void main() {",
                "    val s = true, r = true",
                "    var p = false, q = false",
                "    repeat 3 {",
                "        val newP = q !&& s, newQ = p !&& r",
                "        p = newP",
                "        q = newQ",
                "    }",
                "}"
                ]

        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "main" _ _ blocks _] _], _) -> do
                let bad = findBlockWithTrailingInstrAfterTerminator blocks
                case bad of
                    Nothing -> pure ()
                    Just (bid, termInstr, trailing) ->
                        assertFailure ("block " ++ show bid ++ " has instructions after terminator " ++ show termInstr ++ ": " ++ show trailing)
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]
    where
        findBlockWithTrailingInstrAfterTerminator :: [IRBlock] -> Maybe (Int, IRInstr, [IRInstr])
        findBlockWithTrailingInstrAfterTerminator = findOne
            where
                findOne [] = Nothing
                findOne (IRBlock (bid, body) : rest) = case splitBad body of
                    Just bad -> Just (bid, fst bad, snd bad)
                    Nothing -> findOne rest

                splitBad :: [IRInstr] -> Maybe (IRInstr, [IRInstr])
                splitBad [] = Nothing
                splitBad (instr:xs)
                    | isUnconditionalTerminator instr && not (null xs) = Just (instr, xs)
                    | otherwise = splitBad xs

        isUnconditionalTerminator :: IRInstr -> Bool
        isUnconditionalTerminator instr = case instr of
            Jump {} -> True
            Return -> True
            IReturn -> True
            _ -> False


tests :: TestTree
tests = testGroup "IR.Lowing" [
    topLevelValFinalTests,
    callArgCastTests,
    ternaryControlFlowTests,
    stringLiteralLoweringTests,
    staticFieldReadLoweringTests,
    doubleCmpLoweringTests,
    float128JvmRejectTests,
    incDecLoweringTests,
    assignmentRhsIsolationTests,
    shortCircuitAssignPlacementTests]
