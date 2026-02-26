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
makeErr = makeError "stdin" [makePosition 0 0 0]


decodeValue :: String -> Value
decodeValue s = case decode (BL.pack s) of
    Just v  -> v
    Nothing -> error "Failed to decode JSON from errorToString"


makeErrorTests :: TestTree
makeErrorTests = testGroup "Util.Exception.makeError" [
    testCase "0" $
    makeError "main.x" [makePosition 1 2 3] "unexpected token" @?=
    BasicError { filePath = "main.x", positions = [makePosition 1 2 3], why = "unexpected token"},

    testCase "1" $ makeError "test.x" [makePosition 10 20 1] "lexer error" @?=
    BasicError { filePath = "test.x", positions = [makePosition 10 20 1], why = "lexer error"},

    testCase "2" $ makeError "" [makePosition 0 0 0] "" @?=
    BasicError { filePath = "", positions = [makePosition 0 0 0], why = ""},

    testCase "3" $ makeError "/abs/path/file.x" [makePosition 5 6 7] "syntax error" @?=
    BasicError { filePath = "/abs/path/file.x", positions = [makePosition 5 6 7], why = "syntax error"}]


expectedExpressionTests :: TestTree
expectedExpressionTests = testGroup "Util.Exception.expectedExpression" [
    testCase "0" $ expectedExpression 0 "+" @?= "expected an expression before: '+'",
    testCase "1" $ expectedExpression 0 "-" @?= "expected an expression before: '-'",
    testCase "2" $ expectedExpression 1 "*" @?= "expected an expression after: '*'",
    testCase "3" $ expectedExpression 1 "/" @?= "expected an expression after: '/'"]


getErrorCodeTests :: TestTree
getErrorCodeTests = testGroup "Util.Exception.getErrorCode" [
    testCase "0" $ getErrorCode None @?= 0,
    testCase "1" $ getErrorCode (Unkown "unknown error") @?= (-1),
    testCase "2" $ getErrorCode (Syntax $ makeErr "bad token") @?= 3,
    testCase "3" $ getErrorCode (Parsing $ makeErr "unexpected token") @?= 4]


getErrorMessageTests :: TestTree
getErrorMessageTests = testGroup "Util.Exception.getErrorMessage" [
    testCase "0" $ getErrorMessage None @?= String (T.pack ""),
    testCase "1" $ getErrorMessage (Lexer $ makeErr "test.txt") @?= toJSON (makeErr "test.txt"),
    testCase "2" $ getErrorMessage (Lexer $ makeErr "adfakla") @?= toJSON (makeErr "adfakla"),
    testCase "3" $ getErrorMessage (Lexer $ makeErr "ada.txt") @?= toJSON (makeErr "ada.txt")]


errorToStringTests :: TestTree
errorToStringTests = testGroup "Util.Exception.errorToString" [
    testCase "0" $ decodeValue (errorToString None) @?= object [fromString "code" .= (0 :: Int),  fromString "error" .= getErrorMessage None],
    testCase "1" $ decodeValue (errorToString (Unkown "test.x")) @?= object [fromString "code" .= (-1 :: Int), fromString "error" .= getErrorMessage (Unkown "test.x")],
    testCase "2" $ decodeValue (errorToString (Reading "input.x")) @?= object [fromString "code" .= (1 :: Int),  fromString "error" .= getErrorMessage (Reading "input.x")],
    testCase "3" $ decodeValue (errorToString (Parsing (makeError "p.x" [makePosition 1 2 3] "unexpected token"))) @?= object [fromString "code" .= (4 :: Int),  fromString "error" .= getErrorMessage (Parsing (makeError "p.x" [makePosition 1 2 3] "unexpected token"))]]


tests :: TestTree
tests = testGroup "Util.Exception" [makeErrorTests, expectedExpressionTests, getErrorCodeTests, getErrorMessageTests, errorToStringTests]
