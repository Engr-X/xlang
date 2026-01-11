module Main (main) where

import IC.TestHelper

import qualified Data.TrieTest as DTT

import qualified Lexer.TokenizerTest as LTT

import qualified Util.BasicTest as UBT
import qualified Util.FileHelperTest as UFHT

allTests :: [TestGroup]
allTests = concat [
    LTT.tests,
    DTT.tests, UBT.tests, UFHT.tests]

main :: IO ()
main = runTests allTests