module Data.TrieTest where

import IC.TestHelper

import qualified Data.Trie as UT


testTrie :: UT.TrieTree Char
testTrie = UT.build ["banana", "band", "bat", "apple", "apply", "bandana", "batman", "o", "opq", "anan"]

prefixLengthsTests :: [TestCase]
prefixLengthsTests = map (\(c, e) -> UT.prefixLengths testTrie c --> e) [
    ("", []), ("xyz", []) , ("b", []) , ("ba", []), ("ban", []),
    ("bansadadada", []), ("apricot", []), ("banana", [6]), ("bananab", [6]), ("band", [4]),
    ("bandage", [4]), ("bandband", [4]), ("bandana", [4, 7]) , ("bandanas", [4, 7]), ("bandanaextra", [4, 7]), 
    ("bat", [3]), ("batman", [3, 6]), ("batmanx", [3, 6]), ("batmobile", [3]), ("apple", [5]),
    ("applepie", [5]), ("applyable", [5]), ("o", [1]), ("op", [1]), ("opq", [1, 3]), ("opqsss", [1, 3])]

longestPrefixLengthTests :: [TestCase]
longestPrefixLengthTests = map (\(c, e) -> UT.longestPrefixLength testTrie c --> e) [
        ("", 0), ("xyz", 0) , ("b", 0) , ("ba", 0), ("ban", 0),
        ("bansadadada", 0), ("apricot", 0), ("banana", 6), ("bananab", 6), ("band", 4),
        ("bandage", 4), ("bandband", 4), ("bandana", 7) , ("bandanas", 7), ("bandanaextra", 7), 
        ("bat", 3), ("batman", 6), ("batmanx", 6), ("batmobile", 3), ("apple", 5),
        ("applepie", 5), ("applyable", 5), ("o", 1), ("op", 1), ("opq", 3), ("opqsss", 3)]


eatLongestTests :: [TestCase]
eatLongestTests = map (\(c, e1, e2) -> UT.eatLongest testTrie c --> (e1, e2)) [
    ("", "", ""), ("banana", "banana", ""), ("bandage", "band", "age"), ("batman", "batman", ""), ("bansadadada", "", "bansadadada"),
    ("xyz", "", "xyz"), ("o", "o", ""), ("op", "o", "p"), ("opq", "opq", ""), ("opqsss", "opq", "sss"),
    ("band", "band", ""), ("bandana", "bandana", ""), ("bandanas", "bandana", "s"), ("bat", "bat", ""), ("batmobile", "bat", "mobile"),
    ("apple", "apple", ""), ("applyable", "apply", "able"), ("b", "", "b"), ("ba", "", "ba"), ("ban", "", "ban"),
    ("band", "band", ""), ("bandband", "band", "band"), ("bananab", "banana", "b"), ("apricot", "", "apricot"), ("batmanx", "batman", "x"),
    ("bandanaextra", "bandana", "extra"), ("applepie", "apple", "pie")]
   
containsTests :: [TestCase]
containsTests = map (\(c, e) -> UT.contains testTrie c --> e) [
    ("banana", True), ("band", True), ("bat", True), ("apple", True), ("apply", True),
    ("bandana", True), ("batman", True), ("o", True), ("opq", True),

    ("ban", False), ("bana", False), ("bandan", False), ("batm", False), ("appl", False),
    ("applyy", False), ("op", False), ("opqr", False), ("", False), ("x", False),
    ("xyz", False), ("bandanas", False), ("batmobile", False), ("applepie", False)]

hasPrefixTests :: [TestCase]
hasPrefixTests = map (\(c, e) -> UT.hasPrefix testTrie c --> e) [
    ("banana", True), ("band", True), ("bat", True), ("apple", True), ("apply", True),
    ("bandana", True), ("batman", True), ("o", True), ("opq", True),
    ("ban", True), ("bana", True), ("bandan", True), ("batm", True), ("appl", True),
    ("applyy", False), ("op", True), ("opqr", False), ("", True), ("x", False),
    ("xyz", False), ("bandanas", False), ("batmobile", False), ("applepie", False)]

tests :: [TestGroup]
tests = [
    testGroup "Data.Trie.prefixLengths" prefixLengthsTests,
    testGroup "Data.Trie.longestPrefixLength" longestPrefixLengthTests,

    testGroup "Data.Trie.eatLongest" eatLongestTests,
    testGroup "Data.Trie.contains" containsTests,
    testGroup "Data.Trie.hasPrefix" hasPrefixTests]