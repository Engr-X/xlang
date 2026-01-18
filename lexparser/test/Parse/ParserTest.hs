module Parse.ParserTest where

import Lex.Tokenizer (debugTokenize)
import Test.Tasty
import Test.Tasty.HUnit
import Parse.Parser
import Parse.SyntaxTree
import Util.Type

import qualified Lex.Token as Lex


checkBracketTests :: TestTree
checkBracketTests = testGroup "Parse.Parser.checkBracket" $ map (\(n, s, e) -> 
    let (_, tokens) = debugTokenize s in testCase n $ checkBracket tokens @=? e) [
        ("0", "", Nothing), ("1", "()[]{}", Nothing), ("2 complex", "[() {[], [a..b]} () ]", Nothing),
        ("3", "() )", Just $ Lex.Symbol Lex.RParen $ makePosition 1 4 1), ("4", "[()] ]", Just $ Lex.Symbol Lex.RBracket $ makePosition 1 6 1),
        ("5", "(", Just $ Lex.Symbol Lex.LParen $ makePosition 1 1 1), ("6 missing }", "{ [() ]", Just $ Lex.Symbol Lex.LBrace $ makePosition 1 1 1),
        ("7", "(]", Just $ Lex.Symbol Lex.RBracket $ makePosition 1 2 1)]


classifyNumberTests :: TestTree
classifyNumberTests = testGroup "Parse.Parser.classifyNumber" [
    testCase "0" $ classifyNumber "0" @=? Just (IntConst "0"),
    testCase "1" $ classifyNumber "0xff" @=? Just (IntConst "0xff"),
    testCase "2" $ classifyNumber "0xaal" @=? Just (LongConst "0xaal"),
    testCase "3" $ classifyNumber "123.4" @=? Just (DoubleConst "123.4"),
    testCase "4" $ classifyNumber "1233l" @=? Just (LongConst "1233l"),
    testCase "5" $ classifyNumber "1e9l" @=? Just (LongDoubleConst "1e9l"),
    testCase "5" $ classifyNumber "1e9l" @=? Just (LongDoubleConst "1e9l")]


classifyIdTests :: TestTree
classifyIdTests = testGroup "Parse.Parser.classifyId" [
    testCase "0" $ classifyId "true" @=? BoolConst True,
    testCase "1" $ classifyId "false" @=? BoolConst False,

    testCase "2" $ classifyId "x" @=? Variable "x",
    testCase "3" $ classifyId "flag" @=? Variable "flag",
    testCase "4" $ classifyId "true1" @=? Variable "true1",
    testCase "5" $ classifyId "False" @=? BoolConst False]



tests :: TestTree
tests = testGroup "Parse.Parser" [checkBracketTests, classifyNumberTests, classifyIdTests]
