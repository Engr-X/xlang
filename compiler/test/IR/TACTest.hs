module IR.TACTest where

import IR.TAC
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map


iRet :: IRInstr
iRet = VReturn

iIRet :: IRInstr
iIRet = Return

blk :: Int -> [IRInstr] -> IRBlock
blk bid body = IRBlock (bid, body)


dropTrailingJumpTests :: TestTree
dropTrailingJumpTests = testGroup "IR.TAC.dropTrailingJump" $ map (uncurry testCase) [
    ("0", do
        let inp = [iRet, Jump 2]
        dropTrailingJump inp 2 @?= [iRet]),
    ("1", do
        let inp = [iRet, Jump 2]
        dropTrailingJump inp 3 @?= inp),
    ("2", do
        let inp = [iRet, iIRet]
        dropTrailingJump inp 2 @?= inp),
    ("3", dropTrailingJump [] 1 @?= [])
    ]


pruneBlocksTests :: TestTree
pruneBlocksTests = testGroup "IR.TAC.pruneBlocks" $ map (uncurry testCase) [
    ("0", do
        let inp = [blk 1 [iRet, Jump 2], blk 2 []]
            out = [blk 1 [iRet], blk 2 []]
        pruneBlocks inp @?= out),
    ("1", do
        let inp = [blk 1 [Jump 3], blk 2 []]
        pruneBlocks inp @?= inp),
    ("2", pruneBlocks [] @?= []),
    ("3", do
        let inp = [blk 1 [iRet]]
        pruneBlocks inp @?= inp)
    ]


rmEBInBlocksTests :: TestTree
rmEBInBlocksTests = testGroup "IR.TAC.rmEBInBlocks" $ map (uncurry testCase) [
    ("0", do
        let inp = [blk 1 [], blk 2 [iRet]]
        rmEBInBlocks inp @?= [blk 2 [iRet]]),
    ("1", do
        let inp = [blk 1 []]
        rmEBInBlocks inp @?= inp),
    ("2", do
        let inp = [blk 1 [Jump 2], blk 2 [], blk 3 [iRet]]
        rmEBInBlocks inp @?= [blk 3 [iRet]]),
    ("3", do
        let inp = [blk 1 [iRet], blk 2 [iIRet]]
        rmEBInBlocks inp @?= inp)
    ]


rmEBInFuncTests :: TestTree
rmEBInFuncTests = testGroup "IR.TAC.rmEBInFunc" $ map (uncurry testCase) [
    ("0", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([blk 1 [iRet]], 1) MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([blk 1 [iRet]], 1) MemberClass
        rmEBInFunc f @?= out),
    ("1", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([blk 1 [iRet, iIRet]], 1) MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([blk 1 [iRet, iIRet]], 1) MemberClass
        rmEBInFunc f @?= out),
    ("2", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([blk 1 [], blk 2 [iRet]], 2) MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([blk 2 [iRet]], 2) MemberClass
        rmEBInFunc f @?= out),
    ("3", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([blk 1 [iRet]], 1) MemberClass
        rmEBInFunc f @?= f)
    ]


getInstrSuccsTests :: TestTree
getInstrSuccsTests = testGroup "IR.TAC.getInstrSuccs" $ map (uncurry testCase) [
    ("0", getInstrSuccs (Jump 7) @?= [7]),
    ("1", getInstrSuccs (Ifeq (Int32C 1) (Int32C 1) (3, 5)) @?= [3, 5]),
    ("2", getInstrSuccs (Ifgt (Int32C 2) (Int32C 1) (9, 11)) @?= [9, 11]),
    ("3", getInstrSuccs VReturn @?= [])
    ]


rmEBInBlocksCondRedirectTests :: TestTree
rmEBInBlocksCondRedirectTests = testGroup "IR.TAC.rmEBInBlocksCondRedirect" $ map (uncurry testCase) [
    ("0", do
        let inp = [
                blk 1 [Ifeq (Int32C 1) (Int32C 1) (2, 3)],
                blk 2 [],
                blk 3 [iRet]
                ]
            out = [
                blk 1 [Ifeq (Int32C 1) (Int32C 1) (3, 3)],
                blk 3 [iRet]
                ]
        rmEBInBlocks inp @?= out),
    ("1", do
        let inp = [
                blk 1 [Ifne (Int32C 1) (Int32C 0) (4, 2)],
                blk 2 [],
                blk 4 [iIRet]
                ]
            out = [
                blk 1 [Ifne (Int32C 1) (Int32C 0) (4, 4)],
                blk 4 [iIRet]
                ]
        rmEBInBlocks inp @?= out),
    ("2", do
        let inp = [
                blk 1 [Iflt (Int32C 1) (Int32C 2) (2, 4)],
                blk 2 [],
                blk 3 [],
                blk 4 [iRet]
                ]
            out = [
                blk 1 [Iflt (Int32C 1) (Int32C 2) (4, 4)],
                blk 4 [iRet]
                ]
        rmEBInBlocks inp @?= out),
    ("3", do
        let inp = [blk 1 [Ifge (Int32C 2) (Int32C 1) (9, 8)]]
        rmEBInBlocks inp @?= inp)
    ]


tests :: TestTree
tests = testGroup "IR.TAC" [
    dropTrailingJumpTests,
    pruneBlocksTests,
    rmEBInBlocksTests,
    rmEBInFuncTests,
    getInstrSuccsTests,
    rmEBInBlocksCondRedirectTests
    ]
