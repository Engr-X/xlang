module IR.TACTest where

import IR.TAC
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map


v0 :: IRAtom
v0 = Var ("v", 0, 0)

sRet :: IRStmt
sRet = IRInstr Return

sIRet :: IRStmt
sIRet = IRInstr IReturn

blk :: Int -> [IRStmt] -> IRStmt
blk bid body = IRBlockStmt (IRBlock (bid, body))


dropTrailingJumpTests :: TestTree
dropTrailingJumpTests = testGroup "IR.TAC.dropTrailingJump" $ map (uncurry testCase) [
    ("0", do
        let inp = [sRet, IRInstr (Jump 2)]
        dropTrailingJump inp 2 @?= [sRet]),
    ("1", do
        let inp = [sRet, IRInstr (Jump 2)]
        dropTrailingJump inp 3 @?= inp),
    ("2", do
        let inp = [sRet, sIRet]
        dropTrailingJump inp 2 @?= inp),
    ("3", dropTrailingJump [] 1 @?= [])
    ]


pruneTopStmtsTests :: TestTree
pruneTopStmtsTests = testGroup "IR.TAC.pruneTopStmts" $ map (uncurry testCase) [
    ("0", do
        let inp = [IRInstr (Jump 1), blk 1 []]
        pruneTopStmts inp @?= [blk 1 []]),
    ("1", do
        let inp = [IRInstr (Jump 2), blk 1 []]
        pruneTopStmts inp @?= inp),
    ("2", do
        let inp = [blk 1 [sRet, IRInstr (Jump 2)], blk 2 []]
            out = [blk 1 [sRet], blk 2 []]
        pruneTopStmts inp @?= out),
    ("3", do
        let inp = [blk 1 [IRInstr (Jump 3)], blk 2 []]
        pruneTopStmts inp @?= inp)
    ]


flattenTopStmtsTests :: TestTree
flattenTopStmtsTests = testGroup "IR.TAC.flattenTopStmts" $ map (uncurry testCase) [
    ("0", flattenTopStmts [] @?= []),
    ("1", do
        let inp = [sRet, sIRet]
        flattenTopStmts inp @?= inp),
    ("2", do
        let inp = [blk 1 [sRet, blk 2 [sIRet]]]
            out = [blk 1 [sRet], blk 2 [sIRet]]
        flattenTopStmts inp @?= out),
    ("3", do
        let inp = [sRet, blk 1 [sIRet, blk 2 [sRet]], sIRet]
            out = [sRet, blk 1 [sIRet], blk 2 [sRet], sIRet]
        flattenTopStmts inp @?= out)
    ]


rmEBInStmtsTests :: TestTree
rmEBInStmtsTests = testGroup "IR.TAC.rmEBInStmts" $ map (uncurry testCase) [
    ("0", do
        let inp = [blk 1 [], blk 2 [sRet]]
        rmEBInStmts inp @?= [blk 2 [sRet]]),
    ("1", do
        let inp = [blk 1 []]
        rmEBInStmts inp @?= inp),
    ("2", do
        let inp = [IRInstr (Jump 1), blk 1 [], blk 2 [], blk 3 [sRet]]
        rmEBInStmts inp @?= [blk 3 [sRet]]),
    ("3", do
        let inp = [blk 1 [sRet], blk 2 [sIRet]]
        rmEBInStmts inp @?= inp)
    ]


rmEBInFuncTests :: TestTree
rmEBInFuncTests = testGroup "IR.TAC.rmEBInFunc" $ map (uncurry testCase) [
    ("0", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [sRet]] MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [sRet] MemberClass
        rmEBInFunc f @?= out),
    ("1", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [sRet, sIRet]] MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [sRet, sIRet] MemberClass
        rmEBInFunc f @?= out),
    ("2", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [blk 1 [], blk 2 [sRet]] MemberClass
            out = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [sRet] MemberClass
        rmEBInFunc f @?= out),
    ("3", do
        let f = IRFunction (Public, []) "f" (FunSig [] Void) Map.empty [sRet] MemberClass
        rmEBInFunc f @?= f)
    ]


tests :: TestTree
tests = testGroup "IR.TAC" [
    dropTrailingJumpTests,
    pruneTopStmtsTests,
    flattenTopStmtsTests,
    rmEBInStmtsTests,
    rmEBInFuncTests
    ]
