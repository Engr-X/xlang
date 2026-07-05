module Semantic.OpInferTest where

import Control.Exception (SomeException, evaluate, try)
import Data.List (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree (Class(..), Operator(..))
import Semantic.OpInfer
import Semantic.TypeEnv (FunSig(..))
import Util.Exception (Warning(..))
import Util.Type (makePosition)

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
        ("3", Float64T, Float32T, Float64T),
        ("4", Char, Int8T, Char),
        ("5", Char, Int32T, Char),
        ("6", Char, Int64T, Int64T),
        ("7", Char, Float32T, Float32T),
        ("8", Char, Float64T, Float64T)]


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
        ("1", BitAnd, Char, Int8T, Char),
        ("2", Pow, Float128T, Float128T, Float128T),
        ("3", Mod, Int64T, Int16T, Int64T),
        ("4", Mod, Bool, Bool, Int32T),
        ("5", Add, Pointer Int32T, Int32T, Pointer Int32T),
        ("6", Add, Pointer Int32T, Pointer Int16T, Int64T),
        ("7", Add, Pointer Int32T, Float64T, Float64T),
        ("8", Equal, Pointer Int32T, Int64T, Bool),
        ("9", Add, Char, Int32T, Char),
        ("10", Add, Char, Int64T, Int64T),
        ("11", Add, Char, Float32T, Float32T),
        ("12", Add, Char, Float64T, Float64T)]


inferUnaryOpTests :: TestTree
inferUnaryOpTests = testGroup "Semantic.OpInfer.inferUnaryOp" [
    testCase "0" $ inferUnaryOp LogicalNot Bool @?= Bool,
    testCase "1" $ inferUnaryOp BitInv Int16T @?= Int16T,
    testCase "2" $ inferUnaryOp UnaryMinus Int8T @?= Int32T,
    testCase "3" $ inferUnaryOp SelfInc (Pointer Int32T) @?= Pointer Int32T,
    testCase "4" $ do
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


warnTag :: Warning -> String
warnTag w = case w of
    ImplicitCast _ -> "implicit"
    OverflowWarning _ -> "overflow"
    UnderflowWarning _ -> "underflow"
    Null -> "null"


iCastTests :: TestTree
iCastTests = testGroup "Semantic.OpInfer.iCast" [
    testCase "0 integer widening is silent (int -> long)" $
        iCast "stdin" [makePosition 1 1 1] Int32T Int64T @?= [],
    testCase "1 integer narrowing warns (long -> int)" $
        map warnTag (iCast "stdin" [makePosition 1 1 1] Int64T Int32T) @?= ["implicit", "overflow"],
    testCase "2 pointer<T> -> pointer<void> is silent" $
        iCast "stdin" [makePosition 1 1 1] (Pointer Int32T) (Pointer Void) @?= [],
    testCase "3 pointer<void> -> pointer<T> is silent for null" $
        iCast "stdin" [makePosition 1 1 1] (Pointer Void) (Pointer Int32T) @?= [],
    testCase "4 char to int warns because char ranks above int" $
        map warnTag (iCast "stdin" [makePosition 1 1 1] Char Int32T) @?= ["implicit", "overflow"],
    testCase "5 int to char follows integer widening rank" $
        iCast "stdin" [makePosition 1 1 1] Int32T Char @?= []
    ]


widenedArgsTests :: TestTree
widenedArgsTests = testGroup "Semantic.OpInfer.widenedArgs" [
    testCase "null pointer matches concrete pointer parameter" $
        widenedArgs
            "stdin"
            [makePosition 1 1 1]
            [(Pointer Void, [makePosition 1 3 4])]
            [FunSig [Pointer (Class ["TestCase"] [])] Void]
            @?= Right (FunSig [Pointer (Class ["TestCase"] [])] Void, []),
    testCase "pointer<void> overload is preferred over concrete pointer for null" $
        widenedArgs
            "stdin"
            [makePosition 1 1 1]
            [(Pointer Void, [makePosition 1 3 4])]
            [ FunSig [Pointer (Class ["TestCase"] [])] Int32T
            , FunSig [Pointer Void] Int64T
            ]
            @?= Right (FunSig [Pointer Void] Int64T, []),
    testCase "struct value type sugar matches pointer struct parameter" $
        widenedArgs
            "stdin"
            [makePosition 1 1 1]
            [(Class ["LexPosition"] [], [makePosition 1 3 11])]
            [FunSig [Pointer (Class ["LexPosition"] [])] (Pointer (Class ["LexInput"] []))]
            @?= Right
                ( FunSig [Pointer (Class ["LexPosition"] [])] (Pointer (Class ["LexInput"] []))
                , []
                ),
    testCase "template type names are not normalized to pointers" $
        widenedArgs
            "stdin"
            [makePosition 1 1 1]
            [(Class ["A"] [], [makePosition 1 3 1])]
            [FunSig [Class ["A"] []] (Class ["A"] [])]
            @?= Right (FunSig [Class ["A"] []] (Class ["A"] []), [])
    ]


normalizeTypeAliasSugarTests :: TestTree
normalizeTypeAliasSugarTests = testGroup "Semantic.OpInfer.normalizeTypeAliasSugar" [
    testCase "people => pointer<people>" $
        normalizeTypeAlias (Class ["people"] []) @?= Pointer (Class ["people"] []),
    testCase "LexPosition => pointer<LexPosition>" $
        normalizeTypeAlias (Class ["LexPosition"] []) @?= Pointer (Class ["LexPosition"] []),
    testCase "template A stays type variable" $
        normalizeTypeAlias (Class ["A"] []) @?= Class ["A"] [],
    testCase "pointer<people> stays one-dimensional" $
        normalizeTypeAlias (Pointer (Class ["people"] [])) @?= Pointer (Class ["people"] []),
    testCase "pointer<pointer<people>> stays two-dimensional" $
        normalizeTypeAlias (Pointer (Pointer (Class ["people"] []))) @?= Pointer (Pointer (Class ["people"] []))
    ]


tests :: TestTree
tests = testGroup "Semantic.OpInfer" [isBasicTypeTests, promoteBasicTypeTests, binOpInferTests, inferBinaryOpTests, inferUnaryOpTests, augAssignOpTests, iCastTests, widenedArgsTests, normalizeTypeAliasSugarTests]


