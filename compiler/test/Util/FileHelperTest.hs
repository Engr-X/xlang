module Util.FileHelperTest where

import Test.Tasty
import Test.Tasty.HUnit
import Util.FileHelper

import qualified Util.Exception as UE


classifyErrorTests :: TestTree
classifyErrorTests = testGroup "Util.FileHelper.classifyError" [
    testCase "0" $ classifyError (userError "no such file") @?= UE.Reading "user error (no such file)",
    testCase "0" $ classifyError (userError "fuck") @?= UE.Reading "user error (fuck)",
    testCase "0" $ classifyError (userError "fuck WT") @?= UE.Reading "user error (fuck WT)",
    testCase "0" $ classifyError (userError "fuck BYD") @?= UE.Reading "user error (fuck BYD)"]


readFileTests :: TestTree
readFileTests = testGroup "Util.FileHelper.readFile" [
    testCase "0" $ do
        let path = "test_input.input"
        result <- Util.FileHelper.readFile path
        result @?= Right "世界你好 Hello World\n"]



tests :: TestTree
tests = testGroup "Util.FileHelper" [classifyErrorTests, readFileTests]
