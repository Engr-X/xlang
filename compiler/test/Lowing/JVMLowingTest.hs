module Lowing.JVMLowingTest where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.State.Strict (runState)
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map
import qualified IR.TAC as IR
import qualified Lowing.JVM as JVM
import qualified Lowing.JVMLowing as JL


assertThrowsContains :: String -> a -> Assertion
assertThrowsContains needle thunk = do
    res <- try (evaluate thunk >> pure ()) :: IO (Either SomeException ())
    case res of
        Left ex -> assertBool ("expected error containing: " ++ needle) (needle `isInfixOf` show ex)
        Right _ -> assertFailure ("expected exception containing: " ++ needle)


mkState :: Map IR.IRAtom Class -> JL.LowerState
mkState atomT = JL.LowerState {
    JL.nextLocal = 0,
    JL.locals = Map.empty,
    JL.paramSlots = Map.empty,
    JL.atomTypes = atomT,
    JL.retType = Nothing,
    JL.retLocal = Nothing
}


slotSizeTests :: TestTree
slotSizeTests = testGroup "Lowing.JVMLowing.slotSize" $ map (uncurry testCase) [
    ("0", JL.slotSize Int32T @?= 1),
    ("1", JL.slotSize Int64T @?= 2),
    ("2", JL.slotSize Float64T @?= 2),
    ("3", JL.slotSize (Class ["java", "lang", "String"] []) @?= 1)
    ]


buildParamSlotsTests :: TestTree
buildParamSlotsTests = testGroup "Lowing.JVMLowing.buildParamSlots" $ map (uncurry testCase) [
    ("0", do
        let (mp, nextIdx) = JL.buildParamSlots []
        mp @?= Map.empty
        nextIdx @?= 0),

    ("1", do
        let (mp, nextIdx) = JL.buildParamSlots [Int32T]
        mp @?= Map.fromList [(0, 0)]
        nextIdx @?= 1),

    ("2", do
        let (mp, nextIdx) = JL.buildParamSlots [Int64T, Int32T]
        mp @?= Map.fromList [(0, 0), (1, 2)]
        nextIdx @?= 3),

    ("3", do
        let (mp, nextIdx) = JL.buildParamSlots [Float64T, Int64T]
        mp @?= Map.fromList [(0, 0), (1, 2)]
        nextIdx @?= 4)
    ]


isNoOpCastTests :: TestTree
isNoOpCastTests = testGroup "Lowing.JVMLowing.isNoOpCast" $ map (uncurry testCase) [
    ("0", JL.isNoOpCast Int8T Int32T @?= True),
    ("1", JL.isNoOpCast Int32T Int64T @?= False),
    ("2", JL.isNoOpCast (Class ["A"] []) (Class ["B"] []) @?= True),
    ("3", JL.isNoOpCast (Array Int32T 1) (Class ["Obj"] []) @?= True)
    ]


atomToConstTests :: TestTree
atomToConstTests = testGroup "Lowing.JVMLowing.atomToConst" $ map (uncurry testCase) [
    ("0", JL.atomToConst (IR.Int32C 7) @?= Just (JVM.JI 7)),
    ("1", JL.atomToConst (IR.Float64C 3.5) @?= Just (JVM.JD 3.5)),
    ("2", JL.atomToConst (IR.StringC "hi") @?= Just (JVM.JString "hi")),
    ("3", JL.atomToConst (IR.Var ("x", 0, 0)) @?= Nothing)
    ]


pickCmpClassTests :: TestTree
pickCmpClassTests = testGroup "Lowing.JVMLowing.pickCmpClass" $ map (uncurry testCase) [
    ("0", JL.pickCmpClass Int16T Int32T @?= Int32T),
    ("1", JL.pickCmpClass Int64T Int32T @?= Int64T),
    ("2", JL.pickCmpClass Float32T Int32T @?= Float32T),
    ("3", JL.pickCmpClass Float64T Int64T @?= Float64T)
    ]


cmpOpTests :: TestTree
cmpOpTests = testGroup "Lowing.JVMLowing.cmpOp" $ map (uncurry testCase) [
    ("0", JL.cmpOp JL.CmpEq Int32T 10 @?= JVM.IfcmpEq Int32T 10),
    ("1", JL.cmpOp JL.CmpNe Int64T 11 @?= JVM.IfcmpNe Int64T 11),
    ("2", JL.cmpOp JL.CmpLt Float32T 12 @?= JVM.IfcmpLt Float32T 12),
    ("3", JL.cmpOp JL.CmpGe Float64T 13 @?= JVM.IfcmpGe Float64T 13)
    ]


ensureLocalTests :: TestTree
ensureLocalTests = testGroup "Lowing.JVMLowing.ensureLocal" $ map (uncurry testCase) [
    ("0", do
        let st0 = (mkState Map.empty) { JL.paramSlots = Map.fromList [(0, 2)] }
            (idx, _) = runState (JL.ensureLocal (IR.Param 0)) st0
        idx @?= 2),

    ("1", do
        let v0 = IR.Var ("x", 0, 0)
            st0 = mkState (Map.fromList [(v0, Int32T)])
            (idx, st1) = runState (JL.ensureLocal v0) st0
        idx @?= 0
        JL.nextLocal st1 @?= 1),

    ("2", do
        let v0 = IR.Var ("x", 0, 0)
            st0 = mkState (Map.fromList [(v0, Int32T)])
            (_, st1) = runState (JL.ensureLocal v0) st0
            (idx2, st2) = runState (JL.ensureLocal v0) st1
        idx2 @?= 0
        JL.nextLocal st2 @?= 1),

    ("3", do
        let v0 = IR.Var ("x", 0, 0)
            st0 = mkState (Map.fromList [(v0, Int64T)])
            (_, st1) = runState (JL.ensureLocal v0) st0
        JL.nextLocal st1 @?= 2)
    ]


defaultConstTests :: TestTree
defaultConstTests = testGroup "Lowing.JVMLowing.defaultConst" $ map (uncurry testCase) [
    ("0", do
        let v0 = IR.Var ("x", 0, 0)
            st0 = mkState (Map.fromList [(v0, Int64T)])
            (c, _) = runState (JL.defaultConst v0) st0
        c @?= JVM.JL 0),

    ("1", do
        let v0 = IR.Var ("x", 0, 0)
            st0 = mkState (Map.fromList [(v0, Float32T)])
            (c, _) = runState (JL.defaultConst v0) st0
        c @?= JVM.JF 0),

    ("2", do
        let v0 = IR.Var ("x", 0, 0)
            st0 = mkState (Map.fromList [(v0, Float64T)])
            (c, _) = runState (JL.defaultConst v0) st0
        c @?= JVM.JD 0),

    ("3", do
        let v0 = IR.Var ("x", 0, 0)
            st0 = mkState Map.empty
            (c, _) = runState (JL.defaultConst v0) st0
        c @?= JVM.JI 0)
    ]


jvmLowingFunTests :: TestTree
jvmLowingFunTests = testGroup "Lowing.JVMLowing.jvmLowingFun" $ map (uncurry testCase) [
    ("0", do
        let fun = IR.IRFunction
                (Public, [])
                "main"
                (FunSig [] Int32T)
                Map.empty
                [IR.IRInstr (IR.SetIRet (IR.Int32C 1)), IR.IRInstr IR.IReturn]
                IR.MemberClass
            JVM.JFunction _ _ _ _ body = JL.jvmLowingFun fun
        assertBool "should emit int return op" (any isIntReturn body)),

    ("1", do
        let fun = IR.IRFunction
                (Public, [])
                "main"
                (FunSig [] Void)
                Map.empty
                [IR.IRInstr IR.Return]
                IR.MemberClass
            JVM.JFunction _ _ _ _ body = JL.jvmLowingFun fun
        assertBool "should emit void return op" (any isVoidReturn body)),

    ("2", do
        let v0 = IR.Var ("x", 0, 0)
        let fun = IR.IRFunction
                (Public, [])
                "f"
                (FunSig [] Int32T)
                (Map.fromList [(v0, Int32T)])
                [ IR.IRInstr (IR.IAssign v0 (IR.Int32C 1))
                , IR.IRInstr (IR.SetIRet v0)
                , IR.IRInstr IR.IReturn
                ]
                IR.MemberClass
            JVM.JFunction _ _ _ _ body = JL.jvmLowingFun fun
        assertBool "local value should be loaded before ireturn" (any isLoadLocal body)),

    ("3", do
        let fun = IR.IRFunction
                (Public, [])
                "w"
                (FunSig [] Void)
                Map.empty
                [IR.IRInstr IR.Return]
                IR.MemberClassWrapped
            JVM.JFunction _ _ _ owner _ = JL.jvmLowingFun fun
        owner @?= "xlang-top-level")
    ]
    where
        isIntReturn cmd = case cmd of
            JVM.OP (JVM.ReturnWV Int32T) -> True
            _ -> False

        isVoidReturn cmd = case cmd of
            JVM.OP JVM.Return -> True
            _ -> False

        isLoadLocal cmd = case cmd of
            JVM.OP (JVM.Load Int32T _) -> True
            _ -> False


jvmProgramLoweringTests :: TestTree
jvmProgramLoweringTests = testGroup "Lowing.JVMLowing.jvmProgramLowering" $ map (uncurry testCase) [
    ("0", length (JL.jvmProgmLowing (IR.IRProgm [] [])) @?= 0),

    ("1", do
        let fun = IR.IRFunction (Public, []) "main" (FunSig [] Void) Map.empty [IR.IRInstr IR.Return] IR.MemberClass
            cls = IR.IRClass (Public, []) "MainX" [] (IR.StaticInit []) Map.empty [fun] IR.NoMain
            out = JL.jvmProgmLowing (IR.IRProgm [] [cls])
        case out of
            [JVM.JClass _ qn _ _ _ _ _ methods _] -> do
                qn @?= ["MainX"]
                length methods @?= 1
            _ -> assertFailure ("unexpected class count: " ++ show (length out))),

    ("2", do
        let fun = IR.IRFunction (Public, []) "main" (FunSig [] Void) Map.empty [IR.IRInstr IR.Return] IR.MemberClass
            c1 = IR.IRClass (Public, []) "A" [] (IR.StaticInit []) Map.empty [fun] IR.NoMain
            c2 = IR.IRClass (Public, []) "B" [] (IR.StaticInit []) Map.empty [fun] IR.NoMain
            out = JL.jvmProgmsLowing [IR.IRProgm [] [c1], IR.IRProgm [] [c2]]
        length out @?= 2),

    ("3", do
        let badClass = IR.IRClass
                (Public, [])
                "Bad"
                [((Public, []), Float128T, "x", IR.MemberClass)]
                (IR.StaticInit [])
                Map.empty
                []
                IR.NoMain
        assertThrowsContains "float128 is native-only" (JL.jvmProgmLowing (IR.IRProgm [] [badClass])))
    ]


tests :: TestTree
tests = testGroup "Lowing.JVMLowing" [
    slotSizeTests,
    buildParamSlotsTests,
    isNoOpCastTests,
    atomToConstTests,
    pickCmpClassTests,
    cmpOpTests,
    ensureLocalTests,
    defaultConstTests,
    jvmLowingFunTests,
    jvmProgramLoweringTests
    ]
