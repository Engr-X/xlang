module Main (main) where

import IC.TestHelper

import qualified Lexer.TokenizerTest as LTT

import qualified Util.BasicTest as UBT
import qualified Util.FileHelperTest as UFHT
import qualified Util.TrieTest as UTT

allTests :: [TestGroup]
allTests = concat [
    LTT.tests,
    UTT.tests, UBT.tests, UFHT.tests]

main :: IO ()
main = runTests allTests