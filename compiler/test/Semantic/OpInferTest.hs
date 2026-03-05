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
        ("1", BitLShift, Int8T, Int16T, Just Int32T),
        ("2", BitOr, Bool, Bool, Just Bool),
        ("3", Pow, Int32T, Int32T, Just Float64T)]


inferBinaryOpTests :: TestTree
inferBinaryOpTests = testGroup "Semantic.OpInfer.inferBinaryOp" $
    map (\(name, op, a, b, expected) -> testCase name $ inferBinaryOp op a b @?= expected) [
        ("0", Add, Int16T, Float64T, Float64T),
        ("1", BitAnd, Char, Int8T, Int32T),
        ("2", Pow, Float128T, Float128T, Float128T),
        ("3", Mod, Int64T, Int16T, Int64T)]


augAssignOpTests :: TestTree
augAssignOpTests = testGroup "Semantic.OpInfer.augAssignOp" $
    map (\(name, op, expected) -> testCase name $ augAssignOp op @?= expected) [
        ("0", PlusAssign, Just Add),
        ("1", MinusAssign, Just Sub),
        ("2", BitRShiftAssign, Just BitRShift),
        ("3", Assign, Nothing)]


tests :: TestTree
tests = testGroup "Semantic.OpInfer" [isBasicTypeTests, promoteBasicTypeTests, binOpInferTests, inferBinaryOpTests, augAssignOpTests]
