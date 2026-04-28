module IR.TACLowingTest where

import Control.Monad.State.Strict (evalState)
import IR.TAC (IRAtom(..), IRFunction(..), IRMemberType(..))
import IR.TACLowing
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit
import Util.Type (makePosition)

import qualified Lex.Token as LT
import qualified Data.Map.Strict as Map
import qualified IR.TAC as TAC
import qualified Parse.SyntaxTree as AST
import qualified Semantic.TypeEnv as TEnv


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

normalizeFloatLiteralTests :: TestTree
normalizeFloatLiteralTests = testGroup "IR.TACLowing.normalizeFloatLiteral" $ map (uncurry testCase) [
    ("0", normalizeFloatLiteral ".5f" @?= "0.5"),
    ("1", normalizeFloatLiteral "-.25F" @?= "-0.25"),
    ("2", normalizeFloatLiteral "10." @?= "10.0"),
    ("3", normalizeFloatLiteral "6.02e23" @?= "6.02e23")
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
        let out = detectMainKind ["pkg", "MainX"] [mkFun "main" [Class ["java", "lang", "String"] []] Void]
        out @?= TAC.NoMain)
    ]
    where
        mkFun :: String -> [Class] -> Class -> IRFunction
        mkFun name params ret =
            IRFunction (Public, []) name (FunSig params ret) Map.empty ([], 0) MemberClass


loopAssignKeyTests :: TestTree
loopAssignKeyTests = testGroup "IR.TACLowing.collectAssignKeysBlock" [
    testCase "self-inc is treated as loop-carried assignment key" $ do
        let posI = makePosition 1 1 1
            tokI = LT.Ident "i" posI
            expr = AST.Unary AST.SelfInc (AST.Variable "i" tokI) tokI
            blk = AST.Multiple [AST.Expr expr]
            vUses = Map.fromList [([posI], TEnv.VarLocal (Public, []) "i" 0)]
            st0 = TAC.mkTACState vUses Map.empty
            keys = evalState (TAC.runTACM (collectAssignKeysBlock blk)) st0
        keys @?= [("i", 0)],

    testCase "plus-assign is treated as loop-carried assignment key" $ do
        let posI = makePosition 2 1 1
            posN = makePosition 2 6 1
            tokI = LT.Ident "i" posI
            tokPlusAssign = LT.Symbol LT.PlusAssign (makePosition 2 3 2)
            tokN = LT.NumberConst "1" posN
            expr = AST.Binary AST.PlusAssign (AST.Variable "i" tokI) (AST.IntConst "1" tokN) tokPlusAssign
            blk = AST.Multiple [AST.Expr expr]
            vUses = Map.fromList [([posI], TEnv.VarLocal (Public, []) "i" 0)]
            st0 = TAC.mkTACState vUses Map.empty
            keys = evalState (TAC.runTACM (collectAssignKeysBlock blk)) st0
        keys @?= [("i", 0)]
    ]


shortCircuitLogicalTests :: TestTree
shortCircuitLogicalTests = testGroup "IR.TACLowing.shortCircuitLogical" [
    testCase "logical && lowers to branch form (not IBinary LogicalAnd)" $ do
        let expr = AST.Binary AST.LogicalAnd
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            blocks = lowerExprBlocks expr
            instrs = collectInstrs blocks
        assertBool "&& should contain conditional jump" (any isCondJump instrs)
        assertBool "&& should not emit IBinary LogicalAnd" (not (any isLogicalAndBinary instrs))
        assertBool "&& should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "&& should create branch blocks" (countBlocks blocks >= 3),

    testCase "logical !&& lowers to branch form with !rhs and phi" $ do
        let expr = AST.Binary AST.LogicalNand
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            blocks = lowerExprBlocks expr
            instrs = collectInstrs blocks
        assertBool "!&& should contain conditional jump" (any isCondJump instrs)
        assertBool "!&& should not emit IBinary LogicalNand" (not (any isLogicalNandBinary instrs))
        assertBool "!&& should emit unary logical not for rhs" (any isLogicalNotUnary instrs)
        assertBool "!&& should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "!&& should create branch blocks" (countBlocks blocks >= 3),

    testCase "logical || lowers to branch form (not IBinary LogicalOr)" $ do
        let expr = AST.Binary AST.LogicalOr
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            blocks = lowerExprBlocks expr
            instrs = collectInstrs blocks
        assertBool "|| should contain conditional jump" (any isCondJump instrs)
        assertBool "|| should not emit IBinary LogicalOr" (not (any isLogicalOrBinary instrs))
        assertBool "|| should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "|| should create branch blocks" (countBlocks blocks >= 3),

    testCase "logical !|| lowers to branch form with !rhs and phi" $ do
        let expr = AST.Binary AST.LogicalNor
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            blocks = lowerExprBlocks expr
            instrs = collectInstrs blocks
        assertBool "!|| should contain conditional jump" (any isCondJump instrs)
        assertBool "!|| should not emit IBinary LogicalNor" (not (any isLogicalNorBinary instrs))
        assertBool "!|| should emit unary logical not for rhs" (any isLogicalNotUnary instrs)
        assertBool "!|| should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "!|| should create branch blocks" (countBlocks blocks >= 3),

    testCase "logical -> lowers to branch form (if a then b else true)" $ do
        let expr = AST.Binary AST.LogicalImply
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            blocks = lowerExprBlocks expr
            instrs = collectInstrs blocks
        assertBool "-> should contain conditional jump" (any isCondJump instrs)
        assertBool "-> should not emit IBinary LogicalImply" (not (any isLogicalImplyBinary instrs))
        assertBool "-> should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "-> should create branch blocks" (countBlocks blocks >= 3),

    testCase "logical !-> lowers to branch form (if a then !b else false)" $ do
        let expr = AST.Binary AST.LogicalNimply
                    (AST.BoolConst True LT.dummyToken)
                    (AST.BoolConst False LT.dummyToken)
                    LT.dummyToken
            blocks = lowerExprBlocks expr
            instrs = collectInstrs blocks
        assertBool "!-> should contain conditional jump" (any isCondJump instrs)
        assertBool "!-> should not emit IBinary LogicalNimply" (not (any isLogicalNimplyBinary instrs))
        assertBool "!-> should emit unary logical not for rhs" (any isLogicalNotUnary instrs)
        assertBool "!-> should emit phi-join assignment" (any isPhiAssign instrs)
        assertBool "!-> should create branch blocks" (countBlocks blocks >= 3)
    ]
    where
        lowerExprBlocks expr =
            let st0 = TAC.mkTACState Map.empty Map.empty
                (revStmts, _) = evalState (TAC.runTACM (exprLowing expr)) st0
            in packIRBlocks (reverse revStmts)

        collectInstrs :: [TAC.IRBlock] -> [TAC.IRInstr]
        collectInstrs = concatMap (\(TAC.IRBlock (_, body)) -> body)

        countBlocks :: [TAC.IRBlock] -> Int
        countBlocks = length

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

ifBranchPairTests :: TestTree
ifBranchPairTests = testGroup "IR.TACLowing.ifBranchPair" [
    testCase "if without else should still emit paired branch targets" $ do
        let stmt = AST.If
                (AST.BoolConst True LT.dummyToken)
                (Just (AST.Multiple []))
                Nothing
                (LT.dummyToken, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "if lowering should contain paired Ifeq targets" (any hasPairedIfeq instrs),

    testCase "if with else should emit paired branch targets" $ do
        let stmt = AST.If
                (AST.BoolConst True LT.dummyToken)
                (Just (AST.Multiple []))
                (Just (AST.Multiple []))
                (LT.dummyToken, Just LT.dummyToken)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "if-else lowering should contain paired Ifeq targets" (any hasPairedIfeq instrs)
    ]
    where
        lowerStmtBlocks :: AST.Statement -> [TAC.IRBlock]
        lowerStmtBlocks stmt =
            let st0 = TAC.mkTACState Map.empty Map.empty
                nodes = evalState (TAC.runTACM (stmtsLowing [stmt])) st0
            in packIRBlocks nodes

        collectInstrs :: [TAC.IRBlock] -> [TAC.IRInstr]
        collectInstrs = concatMap (\(TAC.IRBlock (_, body)) -> body)

        hasPairedIfeq :: TAC.IRInstr -> Bool
        hasPairedIfeq instr = case instr of
            TAC.Ifeq _ _ (t, f) -> t /= f
            _ -> False

untilLoweringTests :: TestTree
untilLoweringTests = testGroup "IR.TACLowing.untilLowering" [
    testCase "until condition branches on false" $ do
        let tokUntil = LT.dummyToken
            stmt = AST.Until (AST.BoolConst True LT.dummyToken) Nothing Nothing (tokUntil, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "until should branch when cond == false" (any isEqFalseJump instrs),

    testCase "until should not branch on cond == true" $ do
        let tokUntil = LT.dummyToken
            stmt = AST.Until (AST.BoolConst True LT.dummyToken) Nothing Nothing (tokUntil, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "until should not branch with == true guard" (not (any isEqTrueJump instrs))
    ]
    where
        lowerStmtBlocks :: AST.Statement -> [TAC.IRBlock]
        lowerStmtBlocks stmt =
            let st0 = TAC.mkTACState Map.empty Map.empty
                nodes = evalState (TAC.runTACM (stmtsLowing [stmt])) st0
            in packIRBlocks nodes

        collectInstrs :: [TAC.IRBlock] -> [TAC.IRInstr]
        collectInstrs = concatMap (\(TAC.IRBlock (_, body)) -> body)

        isEqFalseJump :: TAC.IRInstr -> Bool
        isEqFalseJump instr = case instr of
            TAC.Ifeq _ (TAC.Int32C 0) _ -> True
            _ -> False

        isEqTrueJump :: TAC.IRInstr -> Bool
        isEqTrueJump instr = case instr of
            TAC.Ifeq _ (TAC.Int32C 1) _ -> True
            _ -> False

doLoopLoweringTests :: TestTree
doLoopLoweringTests = testGroup "IR.TACLowing.doLoopLowering" [
    testCase "do while condition branches on true" $ do
        let tokDo = LT.dummyToken
            tokWhile = LT.dummyToken
            stmt = AST.DoWhile Nothing (AST.BoolConst True LT.dummyToken) Nothing (tokDo, tokWhile, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "do while should branch with == true guard" (any isEqTrueJump instrs),

    testCase "do while should not branch on false guard" $ do
        let tokDo = LT.dummyToken
            tokWhile = LT.dummyToken
            stmt = AST.DoWhile Nothing (AST.BoolConst True LT.dummyToken) Nothing (tokDo, tokWhile, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "do while should not branch with == false guard" (not (any isEqFalseJump instrs)),

    testCase "do until condition branches on false" $ do
        let tokDo = LT.dummyToken
            tokUntil = LT.dummyToken
            stmt = AST.DoUntil Nothing (AST.BoolConst True LT.dummyToken) Nothing (tokDo, tokUntil, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "do until should branch with == false guard" (any isEqFalseJump instrs),

    testCase "do until should not branch on true guard" $ do
        let tokDo = LT.dummyToken
            tokUntil = LT.dummyToken
            stmt = AST.DoUntil Nothing (AST.BoolConst True LT.dummyToken) Nothing (tokDo, tokUntil, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "do until should not branch with == true guard" (not (any isEqTrueJump instrs))
    ]
    where
        lowerStmtBlocks :: AST.Statement -> [TAC.IRBlock]
        lowerStmtBlocks stmt =
            let st0 = TAC.mkTACState Map.empty Map.empty
                nodes = evalState (TAC.runTACM (stmtsLowing [stmt])) st0
            in packIRBlocks nodes

        collectInstrs :: [TAC.IRBlock] -> [TAC.IRInstr]
        collectInstrs = concatMap (\(TAC.IRBlock (_, body)) -> body)

        isEqFalseJump :: TAC.IRInstr -> Bool
        isEqFalseJump instr = case instr of
            TAC.Ifeq _ (TAC.Int32C 0) _ -> True
            _ -> False

        isEqTrueJump :: TAC.IRInstr -> Bool
        isEqTrueJump instr = case instr of
            TAC.Ifeq _ (TAC.Int32C 1) _ -> True
            _ -> False

repeatLoweringTests :: TestTree
repeatLoweringTests = testGroup "IR.TACLowing.repeatLowering" [
    testCase "repeat counter lowering should not emit phi load for index" $ do
        let tokRepeat = LT.dummyToken
            countExpr = AST.IntConst "10" LT.dummyToken
            stmt = AST.Repeat countExpr Nothing Nothing (tokRepeat, Nothing)
            instrs = collectInstrs (lowerStmtBlocks stmt)
        assertBool "repeat should not emit IAssign _ (Phi ...)" (not (any isPhiAssign instrs))
    ]
    where
        lowerStmtBlocks :: AST.Statement -> [TAC.IRBlock]
        lowerStmtBlocks stmt =
            let st0 = TAC.mkTACState Map.empty Map.empty
                nodes = evalState (TAC.runTACM (stmtsLowing [stmt])) st0
            in packIRBlocks nodes

        collectInstrs :: [TAC.IRBlock] -> [TAC.IRInstr]
        collectInstrs = concatMap (\(TAC.IRBlock (_, body)) -> body)

        isPhiAssign :: TAC.IRInstr -> Bool
        isPhiAssign instr = case instr of
            TAC.IAssign _ (TAC.Phi _) -> True
            _ -> False


pointerSuffixLoweringTests :: TestTree
pointerSuffixLoweringTests = testGroup "IR.TACLowing.pointerSuffix" [
    testCase "a.ref lowers to TAC.Ref and pointer<int>" $ do
        let posA = makePosition 1 1 1
            tokA = LT.Ident "a" posA
            tokRef = LT.Ident "ref" (makePosition 1 3 3)
            expr = AST.Qualified ["a", "ref"] [tokA, tokRef]
            vUses = Map.fromList [([posA], TEnv.VarLocal (Public, []) "a" 0)]
            stacks = Map.fromList [(("a", 0), [(Int32T, 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (instrs, outAtom, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (nodes, atom) <- atomLowing expr
                ty <- TAC.getAtomType atom
                pure (nodes, atom, ty)) st0
        outTy @?= Pointer Int32T
        assertBool "must emit TAC.Ref" (any isRefInstr instrs)
        assertBool "result should be var atom" (isVarAtom outAtom),

    testCase "b.deref.dref lowers to two TAC.Deref and int" $ do
        let posB = makePosition 2 1 1
            tokB = LT.Ident "b" posB
            tokDeref = LT.Ident "deref" (makePosition 2 3 5)
            tokDref = LT.Ident "dref" (makePosition 2 9 4)
            expr = AST.Qualified ["b", "deref", "dref"] [tokB, tokDeref, tokDref]
            bTy = Pointer (Pointer Int32T)
            vUses = Map.fromList [([posB], TEnv.VarLocal (Public, []) "b" 0)]
            stacks = Map.fromList [(("b", 0), [(bTy, 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (instrs, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (nodes, atom) <- atomLowing expr
                ty <- TAC.getAtomType atom
                pure (nodes, ty)) st0
        outTy @?= Int32T
        length (filter isDerefInstr instrs) @?= 2
    ]
  where
    isRefInstr :: IRNode -> Bool
    isRefInstr node = case node of
        IRInstr (TAC.Ref _ _) -> True
        _ -> False

    isDerefInstr :: IRNode -> Bool
    isDerefInstr node = case node of
        IRInstr (TAC.Deref _ _) -> True
        _ -> False

    isVarAtom :: IRAtom -> Bool
    isVarAtom atom = case atom of
        Var _ -> True
        _ -> False

tests :: TestTree
tests = testGroup "IR.TACLowing" [
    stripIntSuffixTests,
    stripFloatSuffixTests,
    normalizeFloatLiteralTests,
    readIntegerLiteralTests,
    wrapIntTests,
    lookupParamIndexTests,
    defaultAtomForClassTests,
    detectMainKindTests,
    loopAssignKeyTests,
    shortCircuitLogicalTests,
    ifBranchPairTests,
    untilLoweringTests,
    doLoopLoweringTests,
    repeatLoweringTests,
    pointerSuffixLoweringTests
    ]




