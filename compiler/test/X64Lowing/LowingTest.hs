module X64Lowing.LowingTest where

import Control.Monad.State.Strict (evalState)
import Data.Map.Strict (Map)
import IR.TAC (IRAtom(..), IRFunction(..), IRInstr(..), IRMemberType(..))
import Parse.SyntaxTree (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit
import X64Lowing.Lowing (runX64LowerState64, x64StmtCode64)

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


tests :: TestTree
tests = testGroup "X64Lowing.LowingTest" [newStackMemChkstkTests, refAssignTests, indirectCallRegTests]
