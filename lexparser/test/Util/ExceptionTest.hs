module Util.ExceptionTest where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Test.Tasty
import Test.Tasty.HUnit
import Util.Exception
import Util.Type

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL


makeErr :: String -> BasicError
makeErr = makeError "stdin" (makePosition 0 0 0)


decodeValue :: String -> Value
decodeValue s = case decode (BL.pack s) of
    Just v  -> v
    Nothing -> error "Failed to decode JSON from errorToString"


makeErrorTests :: TestTree
makeErrorTests = testGroup "Util.Exception.makeError" [
    testCase "Util.Exception.makeError0" $
    makeError "main.x" (makePosition 1 2 3) "unexpected token" @?=
    BasicError { filePath = "main.x", startPosition = makePosition 1 2 3, why = "unexpected token"},

    testCase "Util.Exception.makeError1" $ makeError "test.x" (makePosition 10 20 1) "lexer error" @?=
    BasicError { filePath = "test.x", startPosition = makePosition 10 20 1, why = "lexer error"},

    testCase "Util.Exception.makeError2" $ makeError "" (makePosition 0 0 0) "" @?=
    BasicError { filePath = "", startPosition = makePosition 0 0 0, why = ""},

    testCase "Util.Exception.makeError3" $ makeError "/abs/path/file.x" (makePosition 5 6 7) "syntax error" @?=
    BasicError { filePath = "/abs/path/file.x", startPosition = makePosition 5 6 7, why = "syntax error"}]


getErrorCodeTests :: TestTree
getErrorCodeTests = testGroup "Util.Exception.getErrorCode" [
    testCase "Util.Exception.getErrorCode0" $ getErrorCode None @?= 0,
    testCase "Util.Exception.getErrorCode1" $ getErrorCode (Unkown "unknown error") @?= (-1),
    testCase "Util.Exception.getErrorCode2" $ getErrorCode (Syntax $ makeErr "bad token") @?= 3,
    testCase "Util.Exception.getErrorCode3" $ getErrorCode (Paring $ makeErr "unexpected token") @?= 4]


getErrorMessageTests :: TestTree
getErrorMessageTests = testGroup "Util.Exception.getErrorMessage" [
    testCase "Util.Exception.getErrorMessage0" $ getErrorMessage None @?= String (T.pack ""),
    testCase "Util.Exception.getErrorMessage1" $ getErrorMessage (Lexer $ makeErr "test.txt") @?= toJSON (makeErr "test.txt"),
    testCase "Util.Exception.getErrorMessage2" $ getErrorMessage (Lexer $ makeErr "adfakla") @?= toJSON (makeErr "adfakla"),
    testCase "Util.Exception.getErrorMessage3" $ getErrorMessage (Lexer $ makeErr "ada.txt") @?= toJSON (makeErr "ada.txt")]


errorToStringTests :: TestTree
errorToStringTests = testGroup "Util.Exception.errorToString" [
    testCase "Util.Exception.errorToString0" $ decodeValue (errorToString None) @?=
        object [fromString "code" .= (0 :: Int),  fromString "error" .= getErrorMessage None],

    testCase "Util.Exception.errorToString1" $ decodeValue (errorToString (Unkown "test.x")) @?=
        object [fromString "code" .= (-1 :: Int), fromString "error" .= getErrorMessage (Unkown "test.x")],

    testCase "Util.Exception.errorToString2" $ decodeValue (errorToString (Reading "input.x")) @?=
        object [fromString "code" .= (1 :: Int),  fromString "error" .= getErrorMessage (Reading "input.x")],

    testCase "Util.Exception.errorToString3" $ decodeValue (errorToString (Paring (makeError "p.x" (makePosition 1 2 3) "unexpected token"))) @?=
        object [fromString "code" .= (4 :: Int),  fromString "error" .= getErrorMessage (Paring (makeError "p.x" (makePosition 1 2 3) "unexpected token"))]]


tests :: TestTree
tests = testGroup "Util.Exception" [makeErrorTests, getErrorCodeTests, getErrorMessageTests, errorToStringTests]
