module IR.LowingTest where

import Data.List (find)
import IR.Lowing (codeToIRSingleWithRoot)
import IR.TAC (IRBlock (..), IRClass (..), IRFunction (..), IRInstr (..), IRProgm (..), IRStmt (..))
import Parse.ParserBasic (DeclFlag (..))
import Parse.SyntaxTree (Class (..))
import Test.Tasty
import Test.Tasty.HUnit

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

ternaryControlFlowTests :: TestTree
ternaryControlFlowTests = testGroup "IR.Lowing.ternaryControlFlow" [
    testCase "return-ternary dispatch stays in block join" $ do
        let src = unlines [
                "int f(int x) {",
                "    return -1 if x < 0 else 0",
                "}"
                ]
            isTopIfeq instr = case instr of
                Ifeq {} -> True
                _ -> False
            isTopSetRet instr = case instr of
                SetIRet {} -> True
                _ -> False
            stmtHasIfeq stmt = case stmt of
                IRInstr (Ifeq {}) -> True
                _ -> False
            stmtHasSetRet stmt = case stmt of
                IRInstr (SetIRet {}) -> True
                _ -> False

        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ [IRFunction _ "f" _ _ stmts _] _], _) -> do
                let topInstrs = [instr | IRInstr instr <- stmts]
                    blockBodies = [body | IRBlockStmt (IRBlock (_, body)) <- stmts]
                    topHasIfeq = any isTopIfeq topInstrs
                    topHasSetRet = any isTopSetRet topInstrs
                    blockHasIfeq = any (any stmtHasIfeq) blockBodies
                    blockHasSetRet = any (any stmtHasSetRet) blockBodies

                assertBool "top-level Ifeq should not appear for ternary return" (not topHasIfeq)
                assertBool "top-level SetIRet should not appear for ternary return" (not topHasSetRet)
                assertBool "a block should contain ternary branch Ifeq" blockHasIfeq
                assertBool "a block should contain SetIRet after ternary join" blockHasSetRet
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]


tests :: TestTree
tests = testGroup "IR.Lowing" [
    topLevelValFinalTests,
    ternaryControlFlowTests]
