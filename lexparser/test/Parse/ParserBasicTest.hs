module Parse.ParserBasicTest where

import Lex.Tokenizer (debugTokenize)
import Test.Tasty
import Test.Tasty.HUnit
import Parse.ParserBasic
import Parse.SyntaxTree
import Util.Type

import qualified Lex.Token as Lex


qnameToExprTests :: TestTree
qnameToExprTests = testGroup "Parse.ParserBasic.qnameToExpr" $
    map (\(n, xs, e) -> testCase n $ qnameToExpr xs @=? e) [
        ("0 singleton", ["x"], Variable "x"),
        ("1 singleton empty ident", [""], Variable ""),
        ("2 qualified 2", ["A", "B"], Qualified ["A", "B"]),
        ("3 qualified 3", ["A", "B", "C"], Qualified ["A", "B", "C"])]


nearestTokTests :: TestTree
nearestTokTests = testGroup "Parse.ParserBasic.nearestTok" $ map (\(n, ts, e) -> testCase n $ nearestTok ts @=? e) [
    ("0", [] , Lex.EOF (makePosition 0 0 0)),
    ("1", [Lex.Ident "x" (makePosition 1 1 1)], Lex.Ident "x" (makePosition 1 1 1)),
    ("2", [
        Lex.Symbol Lex.RParen (makePosition 3 7 1), Lex.Ident "y" (makePosition 3 8 1)],
        Lex.Symbol Lex.RParen (makePosition 3 7 1)),
    ("3", [
        Lex.NewLine (makePosition 10 5 1), Lex.Ident "z" (makePosition 11 1 1)],
        Lex.NewLine (makePosition 10 5 1))]


mkHappyErrorExprTests :: TestTree
mkHappyErrorExprTests = testGroup "Parse.ParserBasic.mkHappyErrorExpr" $ map (\(n, ts, e) -> testCase n $ mkHappyErrorExpr ts @=? e) [
    ("0", [], let t = Lex.EOF (makePosition 0 0 0) in Error t ("Parse error near: " ++ show t)),
    ("1", [
        Lex.Ident "x" (makePosition 1 1 1)], let t = Lex.Ident "x" (makePosition 1 1 1) in Error t ("Parse error near: " ++ show t)),
        
    ("2", [
        Lex.Symbol Lex.RBracket (makePosition 2 9 1), Lex.Ident "y" (makePosition 2 10 1)],
        let t = Lex.Symbol Lex.RBracket (makePosition 2 9 1) in Error t ("Parse error near: " ++ show t)), 
    
    ("3 head is NL", [
        Lex.NewLine (makePosition 5 3 1)],
        
        let t = Lex.NewLine (makePosition 5 3 1)
        in Error t ("Parse error near: " ++ show t))]


checkBracketTests :: TestTree
checkBracketTests = testGroup "Parse.ParserBasic.checkBracket" $ map (\(n, s, e) -> 
    let (_, tokens) = debugTokenize s in testCase n $ checkBracket tokens @=? e) [
        ("0", "", Nothing), ("1", "()[]{}", Nothing), ("2 complex", "[() {[], [a..b]} () ]", Nothing),
        ("3", "() )", Just $ Lex.Symbol Lex.RParen $ makePosition 1 4 1), ("4", "[()] ]", Just $ Lex.Symbol Lex.RBracket $ makePosition 1 6 1),
        ("5", "(", Just $ Lex.Symbol Lex.LParen $ makePosition 1 1 1), ("6 missing }", "{ [() ]", Just $ Lex.Symbol Lex.LBrace $ makePosition 1 1 1),
        ("7", "(]", Just $ Lex.Symbol Lex.RBracket $ makePosition 1 2 1)]


classifyNumberTests :: TestTree
classifyNumberTests = testGroup "Parse.ParserBasic.classifyNumber" [
    testCase "0" $ classifyNumber "0" @=? Just (IntConst "0"),
    testCase "1" $ classifyNumber "0xff" @=? Just (IntConst "0xff"),
    testCase "2" $ classifyNumber "0xaal" @=? Just (LongConst "0xaal"),
    testCase "3" $ classifyNumber "123.4" @=? Just (DoubleConst "123.4"),
    testCase "4" $ classifyNumber "1233l" @=? Just (LongConst "1233l"),
    testCase "5" $ classifyNumber "1e9l" @=? Just (LongDoubleConst "1e9l"),
    testCase "5" $ classifyNumber "1e9l" @=? Just (LongDoubleConst "1e9l")]


tests :: TestTree
tests = testGroup "Parse.ParserBasic" [qnameToExprTests, nearestTokTests, mkHappyErrorExprTests, checkBracketTests, classifyNumberTests]
