module IR.TACLowingTest where

import Control.Monad.State.Strict (evalState)
import IR.TAC (IRAtom(..), IRFunction(..), IRMemberType(..))
import IR.TACLowing
import Parse.SyntaxTree (AccessModified(..))
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
            vUses = Map.fromList [([posI], TEnv.VarLocal (Public, []) "i" 0 Int32T)]
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
            vUses = Map.fromList [([posI], TEnv.VarLocal (Public, []) "i" 0 Int32T)]
            st0 = TAC.mkTACState vUses Map.empty
            keys = evalState (TAC.runTACM (collectAssignKeysBlock blk)) st0
        keys @?= [("i", 0)]
    ,
    testCase "post-inc inside assignment lhs index is loop-carried" $ do
        let posDest = makePosition 3 1 4
            posOffset = makePosition 3 6 6
            tokDest = LT.Ident "dest" posDest
            tokOffset = LT.Ident "offset" posOffset
            tokInc = LT.Symbol LT.PlusPlus (makePosition 3 12 2)
            tokAdd = LT.Symbol LT.Plus (makePosition 3 5 1)
            tokAssign = LT.Symbol LT.Assign (makePosition 3 15 1)
            lhs = AST.Unary AST.DeRef
                (AST.Binary AST.Add
                    (AST.Variable "dest" tokDest)
                    (AST.Unary AST.SelfInc (AST.Variable "offset" tokOffset) tokInc)
                    tokAdd)
                tokDest
            expr = AST.Binary AST.Assign lhs (AST.CharConst '1' LT.dummyToken) tokAssign
            blk = AST.Multiple [AST.Expr expr]
            vUses = Map.fromList [
                ([posOffset], TEnv.VarLocal (Public, []) "offset" 0 Int32T)
                ]
            st0 = TAC.mkTACState vUses Map.empty
            keys = evalState (TAC.runTACM (collectAssignKeysBlock blk)) st0
        keys @?= [("offset", 0)]
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
            vUses = Map.fromList [([posA], TEnv.VarLocal (Public, []) "a" 0 Int32T)]
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
            vUses = Map.fromList [([posB], TEnv.VarLocal (Public, []) "b" 0 bTy)]
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

pointerIntrinsicLoweringTests :: TestTree
pointerIntrinsicLoweringTests = testGroup "IR.TACLowing.pointerIntrinsic" [
    testCase "arr.get(i) lowers to pointer read" $ do
        let posArr = makePosition 10 1 3
            posGet = makePosition 10 5 3
            posI = makePosition 10 9 1
            tokArr = LT.Ident "arr" posArr
            tokGet = LT.Ident "get" posGet
            tokI = LT.Ident "i" posI
            expr = AST.Call (AST.Qualified ["arr", "get"] [tokArr, tokGet]) [AST.Variable "i" tokI]
            vUses = Map.fromList
                [ ([posArr], TEnv.VarLocal (Public, []) "arr" 0 (Pointer Int32T))
                , ([posI], TEnv.VarLocal (Public, []) "i" 1 Int32T)
                ]
            stacks = Map.fromList
                [ (("arr", 0), [(Pointer Int32T, 0)])
                , (("i", 1), [(Int32T, 0)])
                ]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Int32T
        assertBool "get should emit TAC.Deref" (any isDerefInstr nodes),

    testCase "arr.set(i, v) lowers to read-old + write and returns element type" $ do
        let posArr = makePosition 11 1 3
            posSet = makePosition 11 5 3
            posI = makePosition 11 9 1
            posV = makePosition 11 12 1
            tokArr = LT.Ident "arr" posArr
            tokSet = LT.Ident "set" posSet
            tokI = LT.Ident "i" posI
            tokV = LT.Ident "v" posV
            expr = AST.Call
                (AST.Qualified ["arr", "set"] [tokArr, tokSet])
                [AST.Variable "i" tokI, AST.Variable "v" tokV]
            vUses = Map.fromList
                [ ([posArr], TEnv.VarLocal (Public, []) "arr" 0 (Pointer Int32T))
                , ([posI], TEnv.VarLocal (Public, []) "i" 1 Int32T)
                , ([posV], TEnv.VarLocal (Public, []) "v" 2 Int32T)
                ]
            stacks = Map.fromList
                [ (("arr", 0), [(Pointer Int32T, 0)])
                , (("i", 1), [(Int32T, 0)])
                , (("v", 2), [(Int32T, 0)])
                ]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Int32T
        assertBool "set should emit TAC.Deref (read old)" (any isDerefInstr nodes)
        assertBool "set should emit TAC.DerefAssign (write new)" (any isDerefAssignInstr nodes),

    testCase "arr[i] = v returns old value (index-set semantics)" $ do
        let posArr = makePosition 12 1 3
            posI = makePosition 12 5 1
            posV = makePosition 12 9 1
            tokArr = LT.Ident "arr" posArr
            tokI = LT.Ident "i" posI
            tokV = LT.Ident "v" posV
            tokLbr = LT.Symbol LT.LBracket (makePosition 12 4 1)
            tokAssign = LT.Symbol LT.Assign (makePosition 12 8 1)
            lhs = AST.Unary AST.DeRef (AST.Binary AST.Add (AST.Variable "arr" tokArr) (AST.Variable "i" tokI) tokLbr) tokLbr
            expr = AST.Binary AST.Assign lhs (AST.Variable "v" tokV) tokAssign
            vUses = Map.fromList
                [ ([posArr], TEnv.VarLocal (Public, []) "arr" 0 (Pointer Int32T))
                , ([posI], TEnv.VarLocal (Public, []) "i" 1 Int32T)
                , ([posV], TEnv.VarLocal (Public, []) "v" 2 Int32T)
                ]
            stacks = Map.fromList
                [ (("arr", 0), [(Pointer Int32T, 0)])
                , (("i", 1), [(Int32T, 0)])
                , (("v", 2), [(Int32T, 0)])
                ]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Int32T
        assertBool "index assign should read old value" (any isDerefInstr nodes)
        assertBool "index assign should still write" (any isDerefAssignInstr nodes),

    testCase "pointer<char> arithmetic scales index by 4 bytes" $ do
        let posP = makePosition 14 1 1
            posOne = makePosition 14 5 1
            tokP = LT.Ident "p" posP
            tokOne = LT.NumberConst "1" posOne
            tokPlus = LT.Symbol LT.Plus (makePosition 14 3 1)
            expr = AST.Binary AST.Add (AST.Variable "p" tokP) (AST.IntConst "1" tokOne) tokPlus
            vUses = Map.fromList [([posP], TEnv.VarLocal (Public, []) "p" 0 (Pointer Char))]
            stacks = Map.fromList [(("p", 0), [(Pointer Char, 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Pointer Char
        assertBool "pointer<char> index must be scaled by sizeof(char)" (any isMulBy4 nodes),

    testCase "blob assignment from string lowers to System.memcopy call" $ do
        let posA = makePosition 13 1 1
            posEq = makePosition 13 3 1
            posS = makePosition 13 5 4
            tokA = LT.Ident "a" posA
            tokEq = LT.Symbol LT.Assign posEq
            tokS = LT.StrConst "Hi" posS
            expr = AST.Binary AST.Assign (AST.Variable "a" tokA) (AST.StringConst "Hi" tokS) tokEq
            vUses = Map.fromList [([posA], TEnv.VarLocal (Public, []) "a" 0 (Blob (AST.IntConst "16" LT.dummyToken)))]
            stacks = Map.fromList [(("a", 0), [(Blob (AST.IntConst "16" LT.dummyToken), 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Blob (AST.IntConst "16" LT.dummyToken)
        assertBool "should call xlang.System.memcopy directly" (any isMemcopyCall nodes)
        assertBool "should not lower as byte-by-byte deref writes anymore" (not (any isBlobByteWrite nodes))
    ]
  where
    isDerefInstr :: IRNode -> Bool
    isDerefInstr node = case node of
        IRInstr (TAC.Deref _ _) -> True
        _ -> False

    isDerefAssignInstr :: IRNode -> Bool
    isDerefAssignInstr node = case node of
        IRInstr (TAC.DerefAssign _ _ _) -> True
        _ -> False

    isBlobByteWrite :: IRNode -> Bool
    isBlobByteWrite node = case node of
        IRInstr (TAC.DerefAssign _ (TAC.Int8C _) 1) -> True
        _ -> False

    isMulBy4 :: IRNode -> Bool
    isMulBy4 node = case node of
        IRInstr (TAC.IBinary _ AST.Mul _ (TAC.Int64C 4)) -> True
        _ -> False

    isMemcopyCall :: IRNode -> Bool
    isMemcopyCall node = case node of
        IRInstr (TAC.ICallStaticDirect _ ["xlang", "System", "memcopy"] [_, _, TAC.Int32C 12]) -> True
        _ -> False

incDecDerefLoweringTests :: TestTree
incDecDerefLoweringTests = testGroup "IR.TACLowing.incDecDeref" [
    testCase "(*p)++ lowers to Deref + DerefAssign and returns element type" $ do
        let posP = makePosition 40 1 1
            tokP = LT.Ident "p" posP
            tokStar = LT.Symbol LT.Multiply (makePosition 40 2 1)
            tokInc = LT.Symbol LT.PlusPlus (makePosition 40 3 2)
            expr = AST.Unary AST.SelfInc (AST.Unary AST.DeRef (AST.Variable "p" tokP) tokStar) tokInc
            vUses = Map.fromList [([posP], TEnv.VarLocal (Public, []) "p" 0 (Pointer Int32T))]
            stacks = Map.fromList [(("p", 0), [(Pointer Int32T, 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Int32T
        assertBool "postfix deref ++ should emit read" (any isDerefInstr nodes)
        assertBool "postfix deref ++ should emit write-back" (any isDerefAssignInstr nodes),

    testCase "++(*p) lowers to Deref + DerefAssign and returns element type" $ do
        let posP = makePosition 41 1 1
            tokP = LT.Ident "p" posP
            tokStar = LT.Symbol LT.Multiply (makePosition 41 3 1)
            tokInc = LT.Symbol LT.PlusPlus (makePosition 41 1 2)
            expr = AST.Unary AST.IncSelf (AST.Unary AST.DeRef (AST.Variable "p" tokP) tokStar) tokInc
            vUses = Map.fromList [([posP], TEnv.VarLocal (Public, []) "p" 0 (Pointer Int32T))]
            stacks = Map.fromList [(("p", 0), [(Pointer Int32T, 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Int32T
        assertBool "prefix deref ++ should emit read" (any isDerefInstr nodes)
        assertBool "prefix deref ++ should emit write-back" (any isDerefAssignInstr nodes)
    ]
  where
    isDerefInstr :: IRNode -> Bool
    isDerefInstr node = case node of
        IRInstr (TAC.Deref _ _) -> True
        _ -> False

    isDerefAssignInstr :: IRNode -> Bool
    isDerefAssignInstr node = case node of
        IRInstr (TAC.DerefAssign _ _ _) -> True
        _ -> False


functionPointerLoweringTests :: TestTree
functionPointerLoweringTests = testGroup "IR.TACLowing.functionPointer" [
    testCase "function reference lowers to TAC.GetFuncAddr with function-pointer type" $ do
        let posAdd = makePosition 30 1 3
            tokAdd = LT.Ident "add" posAdd
            expr = AST.Variable "add" tokAdd
            sig = TEnv.FunSig [Int32T, Int32T] Int32T
            fUses = Map.fromList [([posAdd], TEnv.FunLocal (Public, []) ["add"] sig)]
            st0 = TAC.mkTACState Map.empty fUses
            (nodes, outTy) = evalState (TAC.runTACM $ do
                (lowered, outAtom) <- atomLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= FuncPtr Int32T [Int32T, Int32T]
        assertBool "function reference should emit TAC.GetFuncAddr" (any isGetFuncAddr nodes),

    testCase "calling function-pointer variable lowers to TAC.ICallPtr" $ do
        let posFp = makePosition 31 1 2
            tokFp = LT.Ident "fp" posFp
            tok1 = LT.NumberConst "1" (makePosition 31 4 1)
            tok2 = LT.NumberConst "2" (makePosition 31 7 1)
            expr = AST.Call (AST.Variable "fp" tokFp)
                [AST.IntConst "1" tok1, AST.IntConst "2" tok2]
            fpTy = FuncPtr Int32T [Int32T, Int32T]
            vUses = Map.fromList [([posFp], TEnv.VarLocal (Public, []) "fp" 0 fpTy)]
            stacks = Map.fromList [(("fp", 0), [(fpTy, 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= Int32T
        assertBool "function-pointer call should emit TAC.ICallPtr" (any isICallPtr nodes),

    testCase "assigning through pointer-to-function-pointer lowers to DerefAssign store-size 8" $ do
        let posSlot = makePosition 32 1 4
            posAdd = makePosition 32 10 3
            tokSlot = LT.Ident "slot" posSlot
            tokAdd = LT.Ident "add" posAdd
            tokDeref = LT.Symbol LT.Multiply (makePosition 32 5 1)
            tokAssign = LT.Symbol LT.Assign (makePosition 32 8 1)
            lhs = AST.Unary AST.DeRef (AST.Variable "slot" tokSlot) tokDeref
            rhs = AST.Variable "add" tokAdd
            expr = AST.Binary AST.Assign lhs rhs tokAssign
            fpTy = FuncPtr Int32T [Int32T, Int32T]
            vUses = Map.fromList [([posSlot], TEnv.VarLocal (Public, []) "slot" 0 (Pointer fpTy))]
            fUses = Map.fromList [([posAdd], TEnv.FunLocal (Public, []) ["add"] (FunSig [Int32T, Int32T] Int32T))]
            stacks = Map.fromList [(("slot", 0), [(Pointer fpTy, 0)])]
            st0 = TAC.mkTACState vUses fUses
            (nodes, outTy) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                (lowered, outAtom) <- exprLowing expr
                ty <- TAC.getAtomType outAtom
                pure (lowered, ty)) st0
        outTy @?= fpTy
        assertBool "pointer assignment should emit TAC.DerefAssign with 8-byte store" (any isDerefAssignStore8 nodes)
    ]
  where
    isGetFuncAddr :: IRNode -> Bool
    isGetFuncAddr node = case node of
        IRInstr (TAC.GetFuncAddr _ _) -> True
        _ -> False

    isICallPtr :: IRNode -> Bool
    isICallPtr node = case node of
        IRInstr (TAC.ICallPtr _ _ _) -> True
        _ -> False

    isDerefAssignStore8 :: IRNode -> Bool
    isDerefAssignStore8 node = case node of
        IRInstr (TAC.DerefAssign _ _ 8) -> True
        _ -> False


resolveLiveVarKeyTests :: TestTree
resolveLiveVarKeyTests = testGroup "IR.TACLowing.resolveLiveVarKey" [
    testCase "falls back to in-scope raw variable name when position-mapped key drifts" $ do
        let stacks = Map.fromList
                [ (("src", 3), [(Pointer Int32T, 0)])
                , (("acc", 4), [(Int32T, 0)])
                ]
            st0 = TAC.mkTACState Map.empty Map.empty
            key = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                resolveLiveVarKey "src" ("x", 22)) st0
        key @?= ("src", 3)
    ]


sizeofLoweringTests :: TestTree
sizeofLoweringTests = testGroup "IR.TACLowing.sizeof" [
    testCase "sizeof(type) folds to constant atom" $ do
        let expr = AST.SizeOfType (Int32T, [LT.dummyToken]) LT.dummyToken
            st0 = TAC.mkTACState Map.empty Map.empty
            (nodes, outAtom) = evalState (TAC.runTACM (exprLowing expr)) st0
        nodes @?= []
        outAtom @?= TAC.Int32C 4,

    testCase "sizeof(char) folds to 4 bytes" $ do
        let expr = AST.SizeOfType (Char, [LT.dummyToken]) LT.dummyToken
            st0 = TAC.mkTACState Map.empty Map.empty
            (nodes, outAtom) = evalState (TAC.runTACM (exprLowing expr)) st0
        nodes @?= []
        outAtom @?= TAC.Int32C 4,

    testCase "sizeof(struct type) folds to pointer size" $ do
        let expr = AST.SizeOfType (Class ["Token"] [], [LT.dummyToken]) LT.dummyToken
            st0 = TAC.mkTACState Map.empty Map.empty
            (nodes, outAtom) = evalState (TAC.runTACM (exprLowing expr)) st0
        nodes @?= []
        outAtom @?= TAC.Int32C 8,

    testCase "sizeof(variable) uses type only and folds to constant atom" $ do
        let posA = makePosition 20 1 1
            tokA = LT.Ident "a" posA
            expr = AST.SizeOfExpr (AST.Variable "a" tokA) LT.dummyToken
            vUses = Map.fromList [([posA], TEnv.VarLocal (Public, []) "a" 0 Int64T)]
            stacks = Map.fromList [(("a", 0), [(Int64T, 0)])]
            st0 = TAC.mkTACState vUses Map.empty
            (nodes, outAtom) = evalState (TAC.runTACM $ do
                TAC.setVarStacks stacks
                exprLowing expr) st0
        nodes @?= []
        outAtom @?= TAC.Int32C 8
    ]


structClassSplitTests :: TestTree
structClassSplitTests = testGroup "IR.TACLowing.structClassSplit" [
    testCase "detect struct owner names from generated $$size symbols" $ do
        let stmts =
                [ AST.DefConstVar ["people$$size"] (Just Int32T) (Just (AST.IntConst "32" LT.dummyToken)) [LT.dummyToken]
                , AST.DefConstVar ["node$$size"] (Just Int32T) (Just (AST.IntConst "16" LT.dummyToken)) [LT.dummyToken]
                , AST.DefConstVar ["x"] (Just Int32T) (Just (AST.IntConst "1" LT.dummyToken)) [LT.dummyToken]
                ]
            names = structNamePrefixes stmts
        names @?= ["people", "node"],

    testCase "split struct-owned generated members into dedicated buckets" $ do
        let peopleSize = AST.DefConstVar ["people$$size"] (Just Int32T) (Just (AST.IntConst "32" LT.dummyToken)) [LT.dummyToken]
            peopleInit = AST.Function (Void, [LT.dummyToken]) (AST.Variable "people$__init__" LT.dummyToken) [] (AST.Multiple [])
            mainFun = AST.Function (Void, [LT.dummyToken]) (AST.Variable "main" LT.dummyToken) [] (AST.Multiple [])
            stmts = [peopleSize, peopleInit, mainFun]
            (grouped, plain) = splitStructStmts (structNamePrefixes stmts) stmts
        length (Map.findWithDefault [] "people" grouped) @?= 2
        plain @?= [mainFun],

    testCase "qualified lowered qname keeps explicit struct owner before default file owner" $ do
        let ownerOverrides = Map.fromList
                [ (defaultPlainOwnerKey, "TestsuiteX")
                , ("TestCase$TYPE", "TestCase")
                , ("Testgroup$TYPE", "Testgroup")
                ]
            out = qualifyLoweredQName
                TAC.IRClassTypeStruct
                ownerOverrides
                ["xlang", "test", "Testgroup"]
                ["TestCase$TYPE"]
        out @?= ["xlang", "test", "TestCase", "TestCase$TYPE"],

    testCase "struct IR metadata keeps instance fields for native library import" $ do
        let tokLength = LT.Ident "length" (makePosition 0 2 6)
            metaField =
                AST.DefField
                    [AST.structInstanceFieldMetaName "StringBuilder" "length"]
                    (Just Int32T)
                    Nothing
                    [tokLength]
            irClass = evalState
                (TAC.runTACM $ classStmtsLowing TAC.IRClassTypeStruct [] "StringBuilder" Map.empty [metaField])
                (TAC.mkTACState Map.empty Map.empty)
            structAttrs =
                case irClass of
                    TAC.IRClass _ "StringBuilder" TAC.IRClassTypeStruct attrs _ _ _ _ _ _ -> [attrs]
                    _ -> []
            hasLength attrs =
                any (\(_, cls, name, memberType) ->
                    name == "length" && cls == Int32T && memberType == TAC.MemberClass) attrs

        case structAttrs of
            (attrs:_) -> assertBool "struct metadata should expose instance field length" (hasLength attrs)
            [] -> assertFailure "expected StringBuilder struct IR class"
    ]

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
    resolveLiveVarKeyTests,
    sizeofLoweringTests,
    pointerSuffixLoweringTests,
    pointerIntrinsicLoweringTests,
    incDecDerefLoweringTests,
    functionPointerLoweringTests,
    structClassSplitTests
    ]




