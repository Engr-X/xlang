module X64Lowing.LowingTest where

import Control.Monad.State.Strict (evalState)
import Data.Map.Strict (Map)
import IR.TAC (IRAtom(..), IRBlock(..), IRFunction(..), IRInstr(..), IRMemberType(..))
import Parse.SyntaxTree (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit
import X64Lowing.Lowing (runX64LowerState64, x64LowingFunc, x64StmtCode64)

import qualified Data.Map.Strict as Map
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
    testCase "windows x64 inserts ___chkstk_ms probe call" $ do
        let dst = Var ("dst", 0, 0)
            ir = NewStackMem dst (Int64C 40000)
            fun = mkFun (Map.fromList [(dst, Pointer Void)])
            st = runX64LowerState64 fun 32 X64.winCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasChkstk = any isChkstkCall instrs
        assertBool "expected ___chkstk_ms call for large dynamic stack allocation on Windows" hasChkstk

    , testCase "linux x64 does not insert ___chkstk_ms probe call" $ do
        let dst = Var ("dst", 0, 0)
            ir = NewStackMem dst (Int64C 40000)
            fun = mkFun (Map.fromList [(dst, Pointer Void)])
            st = runX64LowerState64 fun 32 X64.linuxCC64
            instrs = evalState (x64StmtCode64 ir) st
            hasChkstk = any isChkstkCall instrs
        assertBool "did not expect ___chkstk_ms call for non-Windows calling convention" (not hasChkstk)
    ]
    where
        isChkstkCall :: X64.Instruction -> Bool
        isChkstkCall (X64.Call qn _) = qn == X64.mkRawQName64 "___chkstk_ms"
        isChkstkCall _ = False


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
    ]

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
        assertBool "expected function entry to jump into first block label" hasEntryJump
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


tests :: TestTree
tests = testGroup "X64Lowing.LowingTest" [
    newStackMemChkstkTests
    , refAssignTests
    , indirectCallRegTests
    , ifImmCompareTests
    , firstBlockLabelTests
    ]
