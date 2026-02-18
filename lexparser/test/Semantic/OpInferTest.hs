module Semantic.OpInferTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree (Class(..), Operator(..))
import Semantic.OpInfer

import qualified Data.Map.Strict as Map


isBasicTypeTests :: TestTree
isBasicTypeTests = testGroup "Semantic.OpInfer.isBasicType" $
    map (\(name, input, expected) -> testCase name $ isBasicType input @?= expected) [
        ("0", Bool, True),
        ("1", Int32T, True),
        ("2", Array Int32T 3, False),
        ("3", Class ["Foo"] [], False)]


promoteBasicTypeTests :: TestTree
promoteBasicTypeTests = testGroup "Semantic.OpInfer.promoteBasicType" $
    map (\(name, a, b, expected) -> testCase name $ promoteBasicType a b @?= expected) [
        ("0", Int8T, Int16T, Int32T),
        ("1", Int32T, Int64T, Int64T),
        ("2", Float32T, Int16T, Float32T),
        ("3", Float64T, Float32T, Float64T)]


binOpInferTests :: TestTree
binOpInferTests = testGroup "Semantic.OpInfer.binOpInfer" $
    map (\(name, op, a, b, expected) -> testCase name $ Map.lookup (op, a, b) binOpInfer @?= expected) [
        ("0", Equal, Int32T, Float64T, Just Bool),
        ("1", NotEqual, Bool, Char, Just Bool),
        ("2", GreaterThan, Float32T, Int16T, Just Bool),
        ("3", LessEqual, Float128T, Float128T, Just Bool),
        ("4", BitLShift, Int8T, Int16T, Just Int32T),
        ("5", BitRShift, Int64T, Int8T, Just Int64T),
        ("6", BitLShift, Bool, Int32T, Just Int32T),
        ("7", BitRShift, Char, Bool, Just Int32T),
        ("8", BitOr, Bool, Bool, Just Bool),
        ("9", BitXor, Int16T, Int64T, Just Int64T),
        ("10", BitAnd, Char, Int8T, Just Int32T),
        ("11", BitXnor, Int32T, Int32T, Just Int32T),
        ("12", Add, Int16T, Float64T, Just Float64T),
        ("13", Mul, Bool, Bool, Just Int32T),
        ("14", Pow, Int32T, Int32T, Just Float64T),
        ("15", Mod, Int64T, Int16T, Just Int64T)]


augAssignOpTests :: TestTree
augAssignOpTests = testGroup "Semantic.OpInfer.augAssignOp" $
    map (\(name, op, expected) -> testCase name $ augAssignOp op @?= expected) [
        ("0", PlusAssign, Just Add),
        ("1", MinusAssign, Just Sub),
        ("2", BitRShiftAssign, Just BitRShift),
        ("3", Assign, Nothing)]


tests :: TestTree
tests = testGroup "Semantic.OpInfer" [isBasicTypeTests, promoteBasicTypeTests, binOpInferTests, augAssignOpTests]
