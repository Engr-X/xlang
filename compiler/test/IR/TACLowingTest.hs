module IR.TACLowingTest where

import Control.Monad.State.Strict (evalState)
import IR.TAC (IRAtom(..), IRFunction(..), IRMemberType(..))
import IR.TACLowing
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Lex.Token as LT
import qualified Data.Map.Strict as Map
import qualified IR.TAC as TAC
import qualified Parse.SyntaxTree as AST


stripIntSuffixTests :: TestTree
stripIntSuffixTests = testGroup "IR.TACLowing.stripIntSuffix" $ map (uncurry testCase) [
    ("0", stripIntSuffix "10L" @?= "10"),
    ("1", stripIntSuffix "10l" @?= "10"),
    ("2", stripIntSuffix "10" @?= "10"),
    ("3", stripIntSuffix "0xFFL" @?= "0xFF")
    ]


stripFloatSuffixTests :: TestTree
stripFloatSuffixTests = testGroup "IR.TACLowing.stripFloatSuffix" $ map (uncurry testCase) [
    ("0", stripFloatSuffix "1.0f" @?= "1.0"),
    ("1", stripFloatSuffix "2.0F" @?= "2.0"),
    ("2", stripFloatSuffix "3.0L" @?= "3.0"),
    ("3", stripFloatSuffix "4.0" @?= "4.0")
    ]


readIntegerLiteralTests :: TestTree
readIntegerLiteralTests = testGroup "IR.TACLowing.readIntegerLiteral" $ map (uncurry testCase) [
    ("0", readIntegerLiteral "123" @?= Just 123),
    ("1", readIntegerLiteral "0x10" @?= Just 16),
    ("2", readIntegerLiteral "0XffL" @?= Just 255),
    ("3", readIntegerLiteral "0x1g" @?= Nothing)
    ]


wrapIntTests :: TestTree
wrapIntTests = testGroup "IR.TACLowing.wrapInt" $ map (uncurry testCase) [
    ("0", wrapInt (-128, 127) 130 @?= (-126)),
    ("1", wrapInt (-128, 127) (-129) @?= 127),
    ("2", wrapInt (0, 10) 12 @?= 1),
    ("3", wrapInt (0, 10) 5 @?= 5)
    ]


lookupParamIndexTests :: TestTree
lookupParamIndexTests = testGroup "IR.TACLowing.lookupParamIndex" $ map (uncurry testCase) [
    ("0", lookupParamIndex (Int32C 1) Map.empty @?= Nothing),
    ("1", do
        let mp = Map.fromList [(0, Int32C 1), (1, Int32C 2)]
        lookupParamIndex (Int32C 2) mp @?= Just 1),
    ("2", do
        let mp = Map.fromList [(1, Int32C 1), (2, Int32C 1)]
        lookupParamIndex (Int32C 1) mp @?= Just 1),
    ("3", do
        let mp = Map.fromList [(0, Int32C 1)]
        lookupParamIndex (Int32C 3) mp @?= Nothing)
    ]


defaultAtomForClassTests :: TestTree
defaultAtomForClassTests = testGroup "IR.TACLowing.defaultAtomForClass" $ map (uncurry testCase) [
    ("0", defaultAtomForClass Int32T @?= Just (Int32C 0)),
    ("1", defaultAtomForClass Bool @?= Just (BoolC False)),
    ("2", defaultAtomForClass (Class ["java", "lang", "String"] []) @?= Just (StringC "")),
    ("3", defaultAtomForClass (Class ["My", "Type"] []) @?= Nothing)
    ]


detectMainKindTests :: TestTree
detectMainKindTests = testGroup "IR.TACLowing.detectMainKind" $ map (uncurry testCase) [
    ("0", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "notMain" [] Int32T]
        out @?= TAC.NoMain),
    ("1", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "main" [] Int32T]
        out @?= TAC.MainInt ["pkg", "MainX", "main"]),
    ("2", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "main" [] Int32T, mkFun "main" [] Void]
        out @?= TAC.MainVoid ["pkg", "MainX", "main"]),
    ("3", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "main" [Array (Class ["java", "lang", "String"] []) 1] Void]
        out @?= TAC.MainVoidArgs ["pkg", "MainX", "main"])
    ]
    where
        mkFun :: String -> [Class] -> Class -> IRFunction
        mkFun name params ret =
            IRFunction (Public, []) name (FunSig params ret) Map.empty [] MemberClass


shortCircuitLogicalTests :: TestTree
shortCircuitLogicalTests = testGroup "IR.TACLowing.shortCircuitLogical" [
    testCase "logical && lowers to branch form (not IBinary LogicalAnd)" $ do
        let expr = AST.Binary AST.LogicalAnd
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            stmts = lowerExprStmts expr
            instrs = collectInstrs stmts
        assertBool "&& should contain conditional jump" (any isCondJump instrs)
        assertBool "&& should not emit IBinary LogicalAnd" (not (any isLogicalAndBinary instrs))
        assertBool "&& should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "&& should create branch blocks" (countBlocks stmts >= 3),

    testCase "logical !&& lowers to branch form with !rhs and phi" $ do
        let expr = AST.Binary AST.LogicalNand
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            stmts = lowerExprStmts expr
            instrs = collectInstrs stmts
        assertBool "!&& should contain conditional jump" (any isCondJump instrs)
        assertBool "!&& should not emit IBinary LogicalNand" (not (any isLogicalNandBinary instrs))
        assertBool "!&& should emit unary logical not for rhs" (any isLogicalNotUnary instrs)
        assertBool "!&& should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "!&& should create branch blocks" (countBlocks stmts >= 3),

    testCase "logical || lowers to branch form (not IBinary LogicalOr)" $ do
        let expr = AST.Binary AST.LogicalOr
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            stmts = lowerExprStmts expr
            instrs = collectInstrs stmts
        assertBool "|| should contain conditional jump" (any isCondJump instrs)
        assertBool "|| should not emit IBinary LogicalOr" (not (any isLogicalOrBinary instrs))
        assertBool "|| should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "|| should create branch blocks" (countBlocks stmts >= 3),

    testCase "logical !|| lowers to branch form with !rhs and phi" $ do
        let expr = AST.Binary AST.LogicalNor
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            stmts = lowerExprStmts expr
            instrs = collectInstrs stmts
        assertBool "!|| should contain conditional jump" (any isCondJump instrs)
        assertBool "!|| should not emit IBinary LogicalNor" (not (any isLogicalNorBinary instrs))
        assertBool "!|| should emit unary logical not for rhs" (any isLogicalNotUnary instrs)
        assertBool "!|| should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "!|| should create branch blocks" (countBlocks stmts >= 3),

    testCase "logical -> lowers to branch form (if a then b else true)" $ do
        let expr = AST.Binary AST.LogicalImply
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            stmts = lowerExprStmts expr
            instrs = collectInstrs stmts
        assertBool "-> should contain conditional jump" (any isCondJump instrs)
        assertBool "-> should not emit IBinary LogicalImply" (not (any isLogicalImplyBinary instrs))
        assertBool "-> should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "-> should create branch blocks" (countBlocks stmts >= 3),

    testCase "logical !-> lowers to branch form (if a then !b else false)" $ do
        let expr = AST.Binary AST.LogicalNimply
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            stmts = lowerExprStmts expr
            instrs = collectInstrs stmts
        assertBool "!-> should contain conditional jump" (any isCondJump instrs)
        assertBool "!-> should not emit IBinary LogicalNimply" (not (any isLogicalNimplyBinary instrs))
        assertBool "!-> should emit unary logical not for rhs" (any isLogicalNotUnary instrs)
        assertBool "!-> should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "!-> should create branch blocks" (countBlocks stmts >= 3)
    ]
    where
        lowerExprStmts expr =
            let st0 = TAC.mkTACState Map.empty Map.empty
                (revStmts, _) = evalState (TAC.runTACM (exprLowing expr)) st0
            in reverse revStmts

        collectInstrs :: [TAC.IRStmt] -> [TAC.IRInstr]
        collectInstrs = concatMap go
            where
                go stmt = case stmt of
                    TAC.IRInstr instr -> [instr]
                    TAC.IRBlockStmt (TAC.IRBlock (_, body)) -> collectInstrs body

        countBlocks :: [TAC.IRStmt] -> Int
        countBlocks = sum . map go
            where
                go stmt = case stmt of
                    TAC.IRInstr _ -> 0
                    TAC.IRBlockStmt (TAC.IRBlock (_, body)) -> 1 + countBlocks body

        isCondJump :: TAC.IRInstr -> Bool
        isCondJump instr = case instr of
            TAC.Ifeq {} -> True
            TAC.Ifne {} -> True
            TAC.Iflt {} -> True
            TAC.Ifle {} -> True
            TAC.Ifgt {} -> True
            TAC.Ifge {} -> True
            _ -> False

        isLogicalAndBinary :: TAC.IRInstr -> Bool
        isLogicalAndBinary instr = case instr of
            TAC.IBinary _ AST.LogicalAnd _ _ -> True
            _ -> False

        isLogicalNandBinary :: TAC.IRInstr -> Bool
        isLogicalNandBinary instr = case instr of
            TAC.IBinary _ AST.LogicalNand _ _ -> True
            _ -> False

        isLogicalOrBinary :: TAC.IRInstr -> Bool
        isLogicalOrBinary instr = case instr of
            TAC.IBinary _ AST.LogicalOr _ _ -> True
            _ -> False

        isLogicalNorBinary :: TAC.IRInstr -> Bool
        isLogicalNorBinary instr = case instr of
            TAC.IBinary _ AST.LogicalNor _ _ -> True
            _ -> False

        isLogicalImplyBinary :: TAC.IRInstr -> Bool
        isLogicalImplyBinary instr = case instr of
            TAC.IBinary _ AST.LogicalImply _ _ -> True
            _ -> False

        isLogicalNimplyBinary :: TAC.IRInstr -> Bool
        isLogicalNimplyBinary instr = case instr of
            TAC.IBinary _ AST.LogicalNimply _ _ -> True
            _ -> False

        isLogicalNotUnary :: TAC.IRInstr -> Bool
        isLogicalNotUnary instr = case instr of
            TAC.IUnary _ AST.LogicalNot _ -> True
            _ -> False

        isPhiAssign :: TAC.IRInstr -> Bool
        isPhiAssign instr = case instr of
            TAC.IAssign _ (TAC.Phi _) -> True
            _ -> False


tests :: TestTree
tests = testGroup "IR.TACLowing" [
    stripIntSuffixTests,
    stripFloatSuffixTests,
    readIntegerLiteralTests,
    wrapIntTests,
    lookupParamIndexTests,
    defaultAtomForClassTests,
    detectMainKindTests,
    shortCircuitLogicalTests
    ]
