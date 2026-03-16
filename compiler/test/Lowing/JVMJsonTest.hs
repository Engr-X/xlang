module Lowing.JVMJsonTest where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (encode)
import Data.List (isInfixOf)
import Parse.ParserBasic (AccessModified(..), DeclFlag(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified IR.TAC as IR
import qualified Lowing.JVM as JVM
import qualified Lowing.JVMJson as JJ


assertThrowsContains :: String -> a -> Assertion
assertThrowsContains needle thunk = do
    res <- try (evaluate thunk >> pure ()) :: IO (Either SomeException ())
    case res of
        Left ex -> assertBool ("expected error containing: " ++ needle) (needle `isInfixOf` show ex)
        Right _ -> assertFailure ("expected exception containing: " ++ needle)


jsonText :: BL.ByteString -> String
jsonText = BL.unpack


containsInJson :: String -> String -> Assertion
containsInJson needle text =
    assertBool ("expected json to contain: " ++ needle ++ "\nactual: " ++ text) (needle `isInfixOf` text)


typeToQNameTests :: TestTree
typeToQNameTests = testGroup "Lowing.JVMJson.typeToQName" $ map (uncurry testCase) [
    ("0", JJ.typeToQName Int32T @?= ["int32"]),
    ("1", JJ.typeToQName (Array (Class ["java", "lang", "String"] []) 2) @?= ["java", "lang", "String", "[]", "[]"]),
    ("2", assertThrowsContains "generic class types not supported" (JJ.typeToQName (Class ["A"] [Int32T]))),
    ("3", assertThrowsContains "error class" (JJ.typeToQName ErrorClass))
    ]


declToAccessListTests :: TestTree
declToAccessListTests = testGroup "Lowing.JVMJson.declToAccessList" $ map (uncurry testCase) [
    ("0", JJ.declToAccessList (Public, [Static, Final]) @?= ["public", "static", "final"]),
    ("1", JJ.declToAccessList (Private, []) @?= ["private"]),
    ("2", JJ.declToAccessList (Protected, [Final]) @?= ["protected", "final"]),
    ("3", JJ.declToAccessList (Public, []) @?= ["public"])
    ]


opPrefixTests :: TestTree
opPrefixTests = testGroup "Lowing.JVMJson.opPrefix" $ map (uncurry testCase) [
    ("0", JJ.opPrefix Int32T @?= "i"),
    ("1", JJ.opPrefix Float64T @?= "d"),
    ("2", JJ.opPrefix (Class ["A"] []) @?= "a"),
    ("3", JJ.opPrefix (Array Int32T 1) @?= "a")
    ]


cmpPrefixTests :: TestTree
cmpPrefixTests = testGroup "Lowing.JVMJson.cmpPrefix" $ map (uncurry testCase) [
    ("0", JJ.cmpPrefix Int32T @?= "i"),
    ("1", JJ.cmpPrefix Int64T @?= "l"),
    ("2", JJ.cmpPrefix Float32T @?= "f"),
    ("3", JJ.cmpPrefix Float64T @?= "d")
    ]


castOpToJsonTests :: TestTree
castOpToJsonTests = testGroup "Lowing.JVMJson.castOpToJson" $ map (uncurry testCase) [
    ("0", JJ.castOpToJson (Int32T, Float64T) @?= "i2d"),
    ("1", JJ.castOpToJson (Float64T, Int32T) @?= "d2i"),
    ("2", JJ.castOpToJson (Class ["A"] [], Class ["B"] []) @?= "a2a"),
    ("3", JJ.castOpToJson (Int64T, Int64T) @?= "l2l")
    ]


opToJSONTests :: TestTree
opToJSONTests = testGroup "Lowing.JVMJson.opToJSON" $ map (uncurry testCase) [
    ("0", do
        let txt = jsonText (encode (JJ.opToJSON (JVM.Add Int32T)))
        containsInJson "\"op_name\":\"iadd\"" txt),

    ("1", do
        let txt = jsonText (encode (JJ.opToJSON (JVM.Cast Int32T Float64T)))
        containsInJson "\"op_name\":\"i2d\"" txt),

    ("2", do
        let txt = jsonText (encode (JJ.opToJSON (JVM.CPush (JVM.JString "hello"))))
        containsInJson "\"op_name\":\"apush\"" txt
        containsInJson "\"value\":\"hello\"" txt),

    ("3", do
        let txt = jsonText (encode (JJ.opToJSON (JVM.IfcmpGe Float64T 9)))
        containsInJson "\"op_name\":\"if_dcmpge\"" txt
        containsInJson "\"block_id\":9" txt)
    ]


mainKindMetaTests :: TestTree
mainKindMetaTests = testGroup "Lowing.JVMJson.mainKindMeta" $ map (uncurry testCase) [
    ("0", JJ.mainKindMeta IR.NoMain @?= (-1, Nothing)),
    ("1", JJ.mainKindMeta (IR.MainVoid ["MainX", "main"]) @?= (0, Just ["MainX", "main"])),
    ("2", JJ.mainKindMeta (IR.MainInt ["MainX", "main"]) @?= (1, Just ["MainX", "main"])),
    ("3", JJ.mainKindMeta (IR.MainVoidArgs ["MainX", "main"]) @?= (2, Just ["MainX", "main"]))
    ]


jProgmToJSONTests :: TestTree
jProgmToJSONTests = testGroup "Lowing.JVMJson.jProgmToJSON" $ map (uncurry testCase) [
    ("0", do
        let txt = jsonText (encode (JJ.jProgmToJSON 8 []))
        containsInJson "\"classes\":[]" txt),

    ("1", do
        let cls = JVM.JClass
                (Public, [])
                ["MainX"]
                []
                []
                []
                (JVM.JClinit [])
                []
                []
                IR.NoMain
            txt = jsonText (encode (JJ.jProgmToJSON 8 [cls]))
        containsInJson "\"jvm_target\":8" txt
        containsInJson "\"class\":[\"MainX\"]" txt),

    ("2", do
        let cls = JVM.JClass
                (Public, [])
                ["MainX"]
                []
                []
                []
                (JVM.JClinit [])
                []
                []
                (IR.MainVoid ["MainX", "main"])
            txt = jsonText (encode (JJ.jProgmToJSON 8 [cls]))
        containsInJson "\"main_type\":0" txt
        containsInJson "\"main_qname\":[\"MainX\",\"main\"]" txt),

    ("3", do
        let fn = JVM.JFunction
                (Public, [])
                "main"
                (FunSig [] Int32T)
                "xlang-class"
                [JVM.OP (JVM.CPush (JVM.JI 1)), JVM.OP (JVM.ReturnWV Int32T)]
            cls = JVM.JClass
                (Public, [])
                ["MainX"]
                []
                []
                []
                (JVM.JClinit [])
                []
                [fn]
                IR.NoMain
            txt = jsonText (encode (JJ.jProgmToJSON 8 [cls]))
        containsInJson "\"methods\"" txt
        containsInJson "\"name\":\"main\"" txt
        containsInJson "\"return\":[\"int32\"]" txt)
    ]


tests :: TestTree
tests = testGroup "Lowing.JVMJson" [
    typeToQNameTests,
    declToAccessListTests,
    opPrefixTests,
    cmpPrefixTests,
    castOpToJsonTests,
    opToJSONTests,
    mainKindMetaTests,
    jProgmToJSONTests
    ]
