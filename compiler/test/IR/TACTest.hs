module IR.TACTest where

import IR.TAC
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map


iRet :: IRInstr
iRet = Return

iIRet :: IRInstr
iIRet = IReturn

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
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [iRet]] MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [iRet]] MemberClass
        rmEBInFunc f @?= out),
    ("1", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [iRet, iIRet]] MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [iRet, iIRet]] MemberClass
        rmEBInFunc f @?= out),
    ("2", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [], blk 2 [iRet]] MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 2 [iRet]] MemberClass
        rmEBInFunc f @?= out),
    ("3", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [iRet]] MemberClass
        rmEBInFunc f @?= f)
    ]


tests :: TestTree
tests = testGroup "IR.TAC" [
    dropTrailingJumpTests,
    pruneBlocksTests,
    rmEBInBlocksTests,
    rmEBInFuncTests
    ]
