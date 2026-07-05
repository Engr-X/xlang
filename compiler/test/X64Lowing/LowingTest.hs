module X64Lowing.LowingTest where

import Control.Monad.State.Strict (evalState)
import Data.Map.Strict (Map)
import IR.TAC (IRAtom(..), IRBlock(..), IRCFunction(..), IRFunction(..), IRInstr(..), IRMemberType(..))
import Parse.SyntaxTree (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit
import X64Lowing.Lowing (
    lowerCFun,
    runX64LowerState64,
    stStringConstLabelMap64,
    stStructQNameSet64,
    stStructSizeMap64,
    x64LowingFunc,
    x64StmtCode64)

import qualified Lex.Token as LT
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Parse.SyntaxTree as AST
import qualified X64Lowing.ASM as X64


mkFun :: Map IRAtom Class -> IRFunction
mkFun atomTypes =
    IRFunction
        (Public, [])
        "main"
        (FunSig [] Void)
        atomTypes
        ([], 0)
        MemberClassWrapped


newStackMemChkstkTests :: TestTree
newStackMemChkstkTests = testGroup "X64Lowing.NewStackMem.chkstk" [
    testCase "windows x64 dynamic NewStackMem stays self-contained" $ do
        let dst = Var ("dst", 0, 0)
            ir = NewStackMem dst (Int64C 40000)
            fun = mkFun (Map.fromList [(dst, Pointer Void)])
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasChkstk = any isChkstkCall instrs
        assertBool "NewStackMem must not depend on external ___chkstk_ms" (not hasChkstk)

    , testCase "linux x64 does not insert ___chkstk_ms probe call" $ do
        let dst = Var ("dst", 0, 0)
            ir = NewStackMem dst (Int64C 40000)
            fun = mkFun (Map.fromList [(dst, Pointer Void)])
            st = runX64LowerState64 fun 32 X64.linuxCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasChkstk = any isChkstkCall instrs
        assertBool "did not expect ___chkstk_ms call for non-Windows calling convention" (not hasChkstk)
    , testCase "constant blob NewStackMem uses fixed frame slot (no dynamic rsp growth)" $ do
        let dst = Var ("b", 0, 0)
            sz = Var ("sz", 0, 0)
            ir = NewStackMem dst sz
            blobTy = Blob (AST.IntConst "12" LT.dummyToken)
            fun =
                IRFunction
                    (Public, [])
                    "main"
                    (FunSig [] Void)
                    (Map.fromList [(dst, blobTy), (sz, Int64T)])
                    ([IRBlock (0, [ir, VReturn])], 1)
                    MemberClassWrapped
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasRspSubReg = any isRspSubReg instrs
            hasLeaFrame = any isLeaFrame instrs
        assertBool "expected fixed-frame lea for constant blob stack allocation" hasLeaFrame
        assertBool "must not emit dynamic sub rsp,<reg> for fixed blob allocation" (not hasRspSubReg)
    ]
    where
        isChkstkCall :: X64.Instruction -> Bool
        isChkstkCall (X64.Call qn _) = qn == X64.mkRawQName64 "___chkstk_ms"
        isChkstkCall _ = False

        isRspSubReg :: X64.Instruction -> Bool
        isRspSubReg instr = case instr of
            X64.Sub (X64.Reg r1 X64.B64) (X64.Reg _ X64.B64) X64.B64 -> r1 == X64.SP
            _ -> False

        isLeaFrame :: X64.Instruction -> Bool
        isLeaFrame instr = case instr of
            X64.Lea _ (X64.Mem (Just r) _ off) X64.B64 -> r == X64.BP && off < 0
            _ -> False


refAssignTests :: TestTree
refAssignTests = testGroup "X64Lowing.IAssign.ref" [
    testCase "allow pointer<int> -> pointer<long> assign on x64" $ do
        let src = Var ("src", 0, 0)
            dst = Var ("dst", 0, 0)
            ir = IAssign dst src
            fun = mkFun (Map.fromList [(src, Pointer Int32T), (dst, Pointer Int64T)])
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
        assertBool "expected ref assign lowered as mov/mov sequence" (length instrs == 2)
  , testCase "allow deref assign for pointer-to-function-pointer on x64" $ do
        let slot = Var ("slot", 0, 0)
            fn = Var ("fn", 0, 0)
            fpTy = FuncPtr Void [Int32T, Int32T, Char, Int8T]
            ir = DerefAssign slot fn 8
            fun = mkFun (Map.fromList [(slot, Pointer fpTy), (fn, fpTy)])
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasStore64 = any isStore64 instrs
        assertBool "expected 8-byte store for function pointer deref assign" hasStore64
  , testCase "deref assign of string literal stores string address on x64" $ do
        let slot = Var ("slot", 0, 0)
            lit = StringC "a"
            ir = DerefAssign slot lit 8
            fun = mkFun (Map.fromList [(slot, Pointer (Pointer Char))])
            st = (runX64LowerState64 fun 32 X64.winCC64) {
                stStringConstLabelMap64 = Map.fromList [("a", "$LSa")]
                }
            instrs = evalState (x64StmtCode64 ir) st
            hasLeaStringAddr = any isLeaStringAddr instrs
            hasStore64 = any isStore64 instrs
        assertBool "expected lea of string label before pointer store" hasLeaStringAddr
        assertBool "expected 8-byte store for pointer<char> string literal" hasStore64
    ]
  where
    isLeaStringAddr :: X64.Instruction -> Bool
    isLeaStringAddr instr = case instr of
        X64.Lea (X64.Reg _ X64.B64) (X64.Bss _ _ _) X64.B64 -> True
        _ -> False

    isStore64 :: X64.Instruction -> Bool
    isStore64 instr = case instr of
        X64.Mov (X64.Mem _ _ _) _ X64.B64 -> True
        _ -> False


memMoveTmpRegTests :: TestTree
memMoveTmpRegTests = testGroup "X64Lowing.IAssign.mem-mem" [
    testCase "integer mem-to-mem move uses rax as the scratch register" $ do
        let src = Var ("src", 0, 0)
            dst = Var ("dst", 0, 0)
            ir = IAssign dst src
            fun = mkFun (Map.fromList [(src, Int64T), (dst, Int64T)])
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            usesRaxScratch = any isRaxLoad instrs && any isRaxStore instrs
            usesRcxScratch = any isRcxScratch instrs
        assertBool "expected mem-to-mem move to use rax/eax/ax/al scratch" usesRaxScratch
        assertBool "mem-to-mem move should not burn rcx as scratch" (not usesRcxScratch)
    ]
  where
    isRaxLoad :: X64.Instruction -> Bool
    isRaxLoad instr = case instr of
        X64.Mov (X64.Reg X64.A X64.B64) (X64.Mem _ _ _) X64.B64 -> True
        _ -> False

    isRaxStore :: X64.Instruction -> Bool
    isRaxStore instr = case instr of
        X64.Mov (X64.Mem _ _ _) (X64.Reg X64.A X64.B64) X64.B64 -> True
        _ -> False

    isRcxScratch :: X64.Instruction -> Bool
    isRcxScratch instr = case instr of
        X64.Mov (X64.Reg X64.C _) _ _ -> True
        X64.Mov _ (X64.Reg X64.C _) _ -> True
        _ -> False


logicalNotTests :: TestTree
logicalNotTests = testGroup "X64Lowing.IUnary.LogicalNot" [
    testCase "bool logical-not uses byte operations and does not overwrite neighboring stack slots" $ do
        let codeParam = Param 0
            src = Var ("src", 0, 0)
            dst = Var ("dst", 0, 0)
            ir = IUnary dst AST.LogicalNot src
            fun =
                IRFunction
                    (Public, [])
                    "tokenize"
                    (FunSig [Pointer Char] Void)
                    (Map.fromList [(codeParam, Pointer Char), (src, Bool), (dst, Bool)])
                    ([IRBlock (0, [ir, VReturn])], 0)
                    MemberClassWrapped
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasByteXor = any isByteXor instrs
            hasWideBoolWrite = any isWideBoolWrite instrs
        assertBool "expected bool logical-not to use an 8-bit xor" hasByteXor
        assertBool "bool logical-not must not emit 32-bit writes to 1-byte stack slots" (not hasWideBoolWrite)
    ]
  where
    isByteXor :: X64.Instruction -> Bool
    isByteXor instr = case instr of
        X64.Xor (X64.Mem _ _ _) (X64.Imm 1) X64.B8L -> True
        _ -> False

    isWideBoolWrite :: X64.Instruction -> Bool
    isWideBoolWrite instr = case instr of
        X64.Mov (X64.Mem _ _ _) _ X64.B32 -> True
        X64.Xor (X64.Mem _ _ _) _ X64.B32 -> True
        _ -> False

indirectCallRegTests :: TestTree
indirectCallRegTests = testGroup "X64Lowing.ICallPtr.reg" [
    testCase "indirect call keeps rcx arg and uses r10 as call target" $ do
        let fn = Var ("fn", 0, 0)
            a = Var ("a", 0, 0)
            b = Var ("b", 0, 0)
            dst = Var ("dst", 0, 0)
            ir = ICallPtr dst fn [a, b]
            fun = mkFun (Map.fromList [
                (fn, FuncPtr Int32T [Int32T, Int32T]),
                (a, Int32T),
                (b, Int32T),
                (dst, Int32T)])
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasMovFnToR10 = any isMovFnToR10 instrs
            hasCallR10 = any isCallR10 instrs
            clobbersRcXWithFn = any isMovFnToRCX instrs
        assertBool "expected function pointer moved into r10 before indirect call" hasMovFnToR10
        assertBool "expected indirect call through r10" hasCallR10
        assertBool "must not move function pointer into rcx (would clobber arg0)" (not clobbersRcXWithFn)
  ]
  where
    isMovFnToR10 :: X64.Instruction -> Bool
    isMovFnToR10 instr = case instr of
        X64.Mov (X64.Reg X64.R10 X64.B64) _ X64.B64 -> True
        _ -> False

    isCallR10 :: X64.Instruction -> Bool
    isCallR10 instr = case instr of
        X64.CallA (X64.Reg X64.R10 X64.B64) -> True
        _ -> False

    isMovFnToRCX :: X64.Instruction -> Bool
    isMovFnToRCX instr = case instr of
        X64.Mov (X64.Reg X64.C X64.B64) _ X64.B64 -> True
        _ -> False


ifImmCompareTests :: TestTree
ifImmCompareTests = testGroup "X64Lowing.IfImm" [
    testCase "Ifeq with immediates does not emit cmp imm, imm" $ do
        let ir = Ifeq (Int32C 1) (Int32C 1) (1, 2)
            fun = mkFun Map.empty
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasImmImmCmp = any isImmImmCmp instrs
        assertBool "cmp imm, imm is invalid on x64 and must not be emitted" (not hasImmImmCmp)
  ]
  where
    isImmImmCmp :: X64.Instruction -> Bool
    isImmImmCmp instr = case instr of
        X64.Cmp (X64.Imm _) (X64.Imm _) _ -> True
        _ -> False


firstBlockLabelTests :: TestTree
firstBlockLabelTests = testGroup "X64Lowing.FirstBlockLabel" [
    testCase "branch target to first block keeps .L<first> materialized" $ do
        let firstBid = 3
            retBid = 1
            fun =
                IRFunction
                    (Public, [])
                    "main"
                    (FunSig [] Void)
                    Map.empty
                    ([ IRBlock (firstBid, [Ifeq (Int32C 1) (Int32C 1) (firstBid, retBid)])
                     , IRBlock (retBid, [VReturn])
                     ], retBid)
                    MemberClassWrapped
            st = runX64LowerState64 fun 32 X64.winCC64
            (segs, _) = evalState (x64LowingFunc ["MainX"] fun) st
            hasFirstLabel = any isFirstLabel segs
            hasEntryJump = any isEntryJump segs
        assertBool "expected first block label to be emitted when it is a branch target" hasFirstLabel
        assertBool "function entry should fall through into first block label (no redundant jump)" (not hasEntryJump)
  ]
  where
    isFirstLabel :: X64.X64Segment -> Bool
    isFirstLabel seg = case seg of
        X64.X64Label bid _ -> bid == 3
        _ -> False

    isEntryJump :: X64.X64Segment -> Bool
    isEntryJump seg = case seg of
        X64.X64Func _ _ instrs -> any (\i -> i == X64.Jump (Right 3)) instrs
        _ -> False


cLinkMirrorTests :: TestTree
cLinkMirrorTests = testGroup "X64Lowing.CLinkMirror" [
    testCase "link(c) mirror emits thin jmp wrapper only" $ do
        let sig = FunSig [Int32T, Int32T] Int32T
            cfun = IRCFunction (Public, []) "add" sig Map.empty ([], 0) MemberClassWrapped
            st = runX64LowerState64 (mkFun Map.empty) 32 X64.winCC64
            (segs, refs) = evalState (lowerCFun ["KernelX"] Map.empty cfun) st
            expectedTarget = X64.mkRawQName64 (X64.mangleQNameWithSig True ["KernelX", "add"] [Int32T, Int32T, Int32T])
        refs @=? []
        case segs of
            [X64.X64Segement sym [X64.Jump (Left dst)]] -> do
                sym @=? X64.mkRawQName64 "add"
                dst @=? expectedTarget
            _ -> assertFailure ("unexpected c-link mirror segments: " ++ show segs)
  ]

structReturnCallTests :: TestTree
structReturnCallTests = testGroup "X64Lowing.StructReturnCall" [
    testCase "struct-return call reserves stack result area before call" $ do
        let dst = Var ("dst", 0, 0)
            arg0 = Var ("x", 0, 0)
            sTy = AST.Class ["S"] []
            ir = ICallStatic dst ["Demo", "mk"] [arg0]
            fun = mkFun (Map.fromList [(dst, sTy), (arg0, Int32T)])
            st0 = runX64LowerState64 fun 32 X64.winCC64
            st =
                st0 {
                    stStructQNameSet64 = Set.fromList [["Demo", "S"]],
                    stStructSizeMap64 = Map.fromList [(["Demo", "S"], 24)]
                }
            instrs = evalState (x64StmtCode64 ir) st
            hasStructReserve = any isSubRsp64 instrs
            hasHiddenRetPtr = any isLeaR11FromRspPlus32 instrs
            hasShadowRestore = any isAddRsp32 instrs
        assertBool "expected stack reserve (shadow + aligned struct return area) before call" hasStructReserve
        assertBool "expected hidden struct return pointer setup (lea r11, [rsp+32])" hasHiddenRetPtr
        assertBool "expected shadow-space restore after call" hasShadowRestore
  , testCase "pointer-to-struct return uses normal rax return, not hidden sret" $ do
        let dst = Var ("dst", 0, 0)
            arg0 = Var ("x", 0, 0)
            sPtrTy = Pointer (AST.Class ["S"] [])
            ir = ICallStatic dst ["Demo", "get"] [arg0]
            fun = mkFun (Map.fromList [(dst, sPtrTy), (arg0, Int32T)])
            st0 = runX64LowerState64 fun 32 X64.winCC64
            st =
                st0 {
                    stStructQNameSet64 = Set.fromList [["Demo", "S"]],
                    stStructSizeMap64 = Map.fromList [(["Demo", "S"], 24)]
                }
            instrs = evalState (x64StmtCode64 ir) st
            hasStructReserve = any isSubRsp64 instrs
            hasHiddenRetPtr = any isLeaR11FromRspPlus32 instrs
            hasRaxReturnMove = any isRaxReturnMove instrs
        assertBool "pointer return must not reserve a struct-return area" (not hasStructReserve)
        assertBool "pointer return must not pass a hidden return pointer" (not hasHiddenRetPtr)
        assertBool "expected pointer return value to be copied from rax" hasRaxReturnMove
  ]
  where
    isSubRsp64 :: X64.Instruction -> Bool
    isSubRsp64 instr = case instr of
        X64.Sub (X64.Reg X64.SP X64.B64) (X64.Imm 64) X64.B64 -> True
        _ -> False

    isLeaR11FromRspPlus32 :: X64.Instruction -> Bool
    isLeaR11FromRspPlus32 instr = case instr of
        X64.Lea (X64.Reg X64.R11 X64.B64) (X64.Mem (Just X64.SP) Nothing 32) X64.B64 -> True
        _ -> False

    isAddRsp32 :: X64.Instruction -> Bool
    isAddRsp32 instr = case instr of
        X64.Add (X64.Reg X64.SP X64.B64) (X64.Imm 32) X64.B64 -> True
        _ -> False

    isRaxReturnMove :: X64.Instruction -> Bool
    isRaxReturnMove instr = case instr of
        X64.Mov (X64.Mem _ _ _) (X64.Reg X64.A X64.B64) X64.B64 -> True
        _ -> False


tests :: TestTree
tests = testGroup "X64Lowing.LowingTest" [
    newStackMemChkstkTests
    , refAssignTests
    , memMoveTmpRegTests
    , logicalNotTests
    , indirectCallRegTests
    , ifImmCompareTests
    , firstBlockLabelTests
    , cLinkMirrorTests
    , structReturnCallTests
    ]
