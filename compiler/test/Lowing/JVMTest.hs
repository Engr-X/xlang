module Lowing.JVMTest where

import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map
import qualified IR.TAC as IR
import qualified Lowing.JVM as JVM


mkVoidFun :: String -> IR.IRFunction
mkVoidFun name = IR.IRFunction
    (Public, [])
    name
    (FunSig [] Void)
    Map.empty
    [IR.IRBlock (0, [IR.VReturn])]
    IR.MemberClass


mkClass :: String -> [IR.IRFunction] -> IR.IRClass
mkClass name funs = IR.IRClass
    (Public, [])
    name
    []
    (IR.StaticInit [])
    Map.empty
    funs
    IR.NoMain


specialOptimizationTests :: TestTree
specialOptimizationTests = testGroup "Lowing.JVM.specialOptimization" $ map (uncurry testCase) [
    ("0", do
        let ir = IR.IRProgm [] []
        JVM.specialOptimization ir @?= ir),

    ("1", do
        let ir = IR.IRProgm [] [mkClass "MainX" [mkVoidFun "main"]]
        JVM.specialOptimization ir @?= ir),

    ("2", do
        let ir = IR.IRProgm ["pkg"] [mkClass "A" [mkVoidFun "f"], mkClass "B" [mkVoidFun "g"]]
        JVM.specialOptimization ir @?= ir),

    ("3", do
        let attrs = [((Public, []), Int32T, "x", IR.MemberClass)]
            cls = IR.IRClass (Public, []) "C" attrs (IR.StaticInit []) Map.empty [mkVoidFun "h"] IR.NoMain
            ir = IR.IRProgm [] [cls]
        JVM.specialOptimization ir @?= ir)
    ]


tests :: TestTree
tests = testGroup "Lowing.JVM" [specialOptimizationTests]

