module Util.FileHelperTest where

import IC.TestHelper

import qualified Util.FileHelper as UFH


processContentTests :: [TestCase]
processContentTests = map (\(c, e) -> UFH.processContent c --> e) [
    ("\\rline1\r\nline2\r\n\r\nline4\r\n", [(1, "\\rline1"), (2, "line2"), (4, "line4")]),
    ("\r\n\r\n", []),
    ("\nsingle line without newline", [(2, "single line without newline")]),
    ("line with carriage return\r", [(1, "line with carriage return")]),
    ("", [])]

tests :: [TestGroup]
tests = [
    testGroup "Util.FileHelper.processContent" processContentTests]