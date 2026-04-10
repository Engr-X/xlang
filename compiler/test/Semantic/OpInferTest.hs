module Semantic.OpInferTest where

import Control.Exception (SomeException, evaluate, try)
import Data.List (isInfixOf)
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
        ("2", Class ["Vec"] [], False),
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
        ("1", BitLShift, Int8T, Int16T, Just Int16T),
        ("2", BitOr, Bool, Bool, Just Bool),
        ("3", Pow, Int32T, Int32T, Just Float64T),
        ("4", Mod, Bool, Int16T, Just Int32T),
        ("5", Mod, Float32T, Int32T, Nothing)]


inferBinaryOpTests :: TestTree
inferBinaryOpTests = testGroup "Semantic.OpInfer.inferBinaryOp" $
    map (\(name, op, a, b, expected) -> testCase name $ inferBinaryOp op a b @?= expected) [
        ("0", Add, Int16T, Float64T, Float64T),
        ("1", BitAnd, Char, Int8T, Int8T),
        ("2", Pow, Float128T, Float128T, Float128T),
        ("3", Mod, Int64T, Int16T, Int64T),
        ("4", Mod, Bool, Bool, Int32T)]


inferUnaryOpTests :: TestTree
inferUnaryOpTests = testGroup "Semantic.OpInfer.inferUnaryOp" [
    testCase "0" $ inferUnaryOp LogicalNot Bool @?= Bool,
    testCase "1" $ inferUnaryOp BitInv Int16T @?= Int16T,
    testCase "2" $ inferUnaryOp UnaryMinus Int8T @?= Int32T,
    testCase "3" $ do
        res <- try (evaluate (inferUnaryOp LogicalNot Float32T)) :: IO (Either SomeException Class)
        case res of
            Left ex -> assertBool "should report unsupported unary operator" ("unsupported unary operator" `isInfixOf` show ex)
            Right t -> assertFailure ("expected exception, got type: " ++ show t)
    ]


augAssignOpTests :: TestTree
augAssignOpTests = testGroup "Semantic.OpInfer.augAssignOp" $
    map (\(name, op, expected) -> testCase name $ augAssignOp op @?= expected) [
        ("0", PlusAssign, Just Add),
        ("1", MinusAssign, Just Sub),
        ("2", PowerAssign, Just Pow),
        ("3", Assign, Nothing)]


tests :: TestTree
tests = testGroup "Semantic.OpInfer" [isBasicTypeTests, promoteBasicTypeTests, binOpInferTests, inferBinaryOpTests, inferUnaryOpTests, augAssignOpTests]


