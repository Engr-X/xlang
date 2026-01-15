module Lexer.TokenizerTest where


import Lexer.Token
import Lexer.Tokenizer

import Test.Tasty
import Test.Tasty.HUnit

import Util.Types
import Util.Exception (ErrorKind)


name :: String
name = "Lexer.Tokenizer.tokenize"


makeTest :: Int -> String -> ([ErrorKind], [Token]) -> TestTree
makeTest i input expect = let name' = name ++ show i in testCase name' $ debugTokenize input @=? expect

makeId :: String -> Int -> Int -> Int -> Token
makeId s a b c = Ident s $ makePosition a b c

makeSymbol :: Symbol -> Int -> Int -> Int -> Token
makeSymbol s a b c = Symbol s $ makePosition a b c

makeNum :: String -> Int -> Int -> Int -> Token
makeNum s a b c = NumberConst s $ makePosition a b c

makeChar :: Char -> Int -> Int -> Int -> Token
makeChar c a b c' = CharConst c $ makePosition a b c'

makeStr :: String -> Int -> Int -> Int -> Token
makeStr str a b c = StrConst str $ makePosition a b c


datas :: [(String, ([ErrorKind], [Token]))]
datas = [
{-
int x = 1, y = 2.0f, c = '\0'; // some basic val
-}
    ("int x = 1, y = 2.0f, c = '\\0'; // some basic val", ([], [
        makeId "int" 1 1 3,
        makeId "x" 1 5 1, makeSymbol Assign 1 7 1, makeNum "1" 1 9 1, makeSymbol Comma 1 10 1,
        makeId "y" 1 12 1, makeSymbol Assign 1 14 1, makeNum "2.0f" 1 16 4, makeSymbol Comma 1 20 1,
        makeId "c" 1 22 1, makeSymbol Assign 1 24 1, makeChar '\0' 1 26 4, makeSymbol Semicolon 1 30 1])),

{-
a='\n'; /* this is crazy
*/
b='\t'; // this is crazy too~
c='\''; // this is crazy 2
d='\\'; // this is awesome

-}       
    (unlines [
        "a='\\n'; /* this is crazy",
        "*/",
        "b='\\t'; // this is crazy too~",
        "c='\\''; // this is crazy 2",
        "d='\\\\'; // this is awesome"], ([], [
        makeId "a" 1 1 1,  makeSymbol Assign 1 2 1,  makeChar '\n' 1 3 4,  makeSymbol Semicolon 1 7 1,
        makeId "b" 3 1 1,  makeSymbol Assign 3 2 1,  makeChar '\t' 3 3 4,  makeSymbol Semicolon 3 7 1,
        makeId "c" 4 1 1,  makeSymbol Assign 4 2 1,  makeChar '\'' 4 3 4,  makeSymbol Semicolon 4 7 1,
        makeId "d" 5 1 1,  makeSymbol Assign 5 2 1,  makeChar '\\' 5 3 4,  makeSymbol Semicolon 5 7 1])),

{-
str1 = "hello, this is my own program language!!!";
str2 = "hello, world in Chinese is 世界你好";
/* this is a test */ str3 = "\0";
/* /* perfect */
str4 = null
-}
    (unlines [
        "str1 = \"hello, this is my own program language!!!\";",
        "str2 = \"hello, world in Chinese is \19990\30028\20320\22909\";",
        "/* this is a test */ str3 = \"\\0\";",
        "/* /* perfect */",
        "str4 = null"], ([], [
        makeId "str1" 1 1 4,  makeSymbol Assign 1 6 1,  makeStr "hello, this is my own program language!!!" 1 8 43,  makeSymbol Semicolon 1 51 1,
        makeId "str2" 2 1 4,  makeSymbol Assign 2 6 1,  makeStr "hello, world in Chinese is 世界你好" 2 8 33,  makeSymbol Semicolon 2 41 1,

        makeId "str3" 3 22 4,  makeSymbol Assign 3 27 1,  makeStr  "\\0" 3 29 4, makeSymbol Semicolon 3 33 1,
        makeId "str4" 5 1 4,  makeSymbol Assign 5 6 1,  makeId "null" 5 8 4
    ])),

{-
for (x = a_address.getSize(); x <= 10; x *= 2) /* iterate the list */
    print(x) # this is a comment too
-}
    (unlines [
        "for (x = a_address.getSize(); x <= 10; x *= 2) /* iterate the list */",
        "    print(x) # this is a comment too"], ([], [
        makeId "for" 1 1 3, makeSymbol LParen 1 5 1, makeId "x" 1 6 1, makeSymbol Assign 1 8 1, makeId "a_address" 1 10 9, makeSymbol Dot 1 19 1, makeId "getSize" 1 20 7, makeSymbol LParen 1 27 1, makeSymbol LParen 1 28 1, makeSymbol Semicolon 1 29 1,
            makeId "x" 1 31 1, makeSymbol LessEqual 1 33 1, makeNum "10" 1 36 2, makeSymbol Semicolon 1 38 1,
            makeId "x" 1 40 1, makeSymbol MultiplyAssign 1 42 2, makeNum "2" 1 45 1, makeSymbol Semicolon 1 46 1,
        makeId "print" 2 4 5, makeSymbol LParen 2 10 1, makeId "x" 2 11 1, makeSymbol RParen 2 12 1
    ]))]



tests :: TestTree
tests = testGroup name $ zipWith (\idx (input, expect) -> makeTest idx input expect) [0..] datas

