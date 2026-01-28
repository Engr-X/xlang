module Parse.ParserBasicTest where

import Lex.Tokenizer (replTokenize)
import Test.Tasty
import Test.Tasty.HUnit
import Lex.Token (Token, tokenPos)
import Parse.ParserBasic
import Parse.SyntaxTree
import Util.Type

import qualified Lex.Token as Lex
import qualified Util.Exception as UE


qnameToExprTests :: TestTree
qnameToExprTests = testGroup "Parse.ParserBasic.qnameToExpr" $ map (\(n, xs, ts, e) -> testCase n $ qnameToExpr (xs, ts) @=? e) [
    ("0", ["x"], [tokX], Variable "x" tokX),
    ("1", [""], [tokEmpty], Variable "" tokEmpty),
    ("2" , reverse ["A", "B"], reverse [tokA, tokB], Qualified ["A", "B"] [tokA, tokB]),
    ("3", reverse ["A", "B", "C"], reverse [tokA, tokB, tokC], Qualified ["A", "B", "C"] [tokA, tokB, tokC])]
    where
        tokX, tokEmpty, tokA, tokB, tokC :: Token
        tokX     = Lex.Ident "x"     (makePosition 1 1 1)
        tokEmpty = Lex.Ident ""      (makePosition 1 1 0)
        tokA     = Lex.Ident "A"     (makePosition 1 1 1)
        tokB     = Lex.Ident "B"     (makePosition 1 3 1)
        tokC     = Lex.Ident "C"     (makePosition 1 5 1)


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
    let (_, tokens) = replTokenize s in testCase n $ checkBracket tokens @=? e) [
        ("0", "", Nothing), ("1", "()[]{}", Nothing), ("2 complex", "[() {[], [a..b]} () ]", Nothing),
        ("3", "() )", Just $ Lex.Symbol Lex.RParen $ makePosition 1 4 1), ("4", "[()] ]", Just $ Lex.Symbol Lex.RBracket $ makePosition 1 6 1),
        ("5", "(", Just $ Lex.Symbol Lex.LParen $ makePosition 1 1 1), ("6 missing }", "{ [() ]", Just $ Lex.Symbol Lex.LBrace $ makePosition 1 1 1),
        ("7", "(]", Just $ Lex.Symbol Lex.RBracket $ makePosition 1 2 1)]


classifyNumberTests :: TestTree
classifyNumberTests = testGroup "Parse.ParserBasic.classifyNumber" $ map (\(i, s, e) -> testCase i $ classifyNumber s (mkTok s) @=? e) [
    ("int", "0", Just (IntConst "0" (mkTok "0"))),
    ("double", "123.4", Just (DoubleConst "123.4" (mkTok "123.4"))),
    ("longdouble", "1e9l", Just (LongDoubleConst "1e9l" (mkTok "1e9l"))),
    ("invalid", "0x", Nothing)]
    where
        mkTok :: String -> Token
        mkTok s = Lex.NumberConst s $ makePosition 1 1 (length s)


toExceptionTests :: TestTree
toExceptionTests = testGroup "toException" $ map (\(i, expr, expected) ->
    testCase i $ toException "stdin" expr @=? expected) [
    ("0",
        Error tokBad "boom",
        UE.Parsing (UE.makeError "stdin" (tokenPos tokBad) "boom")),
        
    ("1",
        Error tokPlus "bad plus",
        UE.Parsing (UE.makeError "stdin" (tokenPos tokPlus) "bad plus")),
        
    ("2",
        Error tokNum "bad number",
        UE.Parsing (UE.makeError "stdin" (tokenPos tokNum) "bad number")),
        
    ("3",
        Error tokId "bad ident",
        UE.Parsing (UE.makeError "stdin" (tokenPos tokId) "bad ident"))]
    where
        tokBad, tokNum, tokId, tokPlus :: Token
        tokBad  = Lex.Symbol Lex.RBrace (makePosition 1 4 1)
        tokNum  = Lex.NumberConst "1"   (makePosition 1 1 1)
        tokId   = Lex.Ident "x"         (makePosition 1 1 1)
        tokPlus = Lex.Symbol Lex.Plus   (makePosition 1 3 1)


stmtToBlockTests :: TestTree
stmtToBlockTests = testGroup "Parse.ParserBasic.stmtToBlock" $
    map (\(name, stmt, expected) -> testCase name $ stmtToBlock stmt @=? expected) [
        ("0",
            Expr (IntConst "1" (mkNum "1" 1 1 1)),
            Multiple [
                Expr (IntConst "1" (mkNum "1" 1 1 1))
            ]),

        ("1",
            BlockStmt (Multiple [
                Expr (IntConst "1" (mkNum "1" 1 1 1))
            ]),
            Multiple [
                Expr (IntConst "1" (mkNum "1" 1 1 1))
            ]),

        ("2",
            While
                (BoolConst True (mkId "true" 1 1 4))
                (Just (Multiple [
                    Expr (IntConst "1" (mkNum "1" 2 1 1))
                ])),
            Multiple [
                While
                    (BoolConst True (mkId "true" 1 1 4))
                    (Just (Multiple [
                        Expr (IntConst "1" (mkNum "1" 2 1 1))
                    ]))]),

        ("3",
            While
                (BoolConst True (mkId "true" 1 1 4))
                Nothing,
            Multiple [
                While
                    (BoolConst True (mkId "true" 1 1 4))
                    Nothing])]
    where
        -- mkSym :: Symbol -> Int -> Int -> Int -> Token
        -- mkSym s a b c = Lex.Symbol s $ Position a b c

        mkNum :: String -> Int -> Int -> Int -> Token
        mkNum s a b c = Lex.NumberConst s $ Position a b c

        mkId :: String -> Int -> Int -> Int -> Token
        mkId s a b c = Lex.Ident s $ makePosition a b c


tests :: TestTree
tests = testGroup "Parse.ParserBasic" [
    qnameToExprTests, nearestTokTests, mkHappyErrorExprTests, classifyNumberTests, toExceptionTests, stmtToBlockTests]
