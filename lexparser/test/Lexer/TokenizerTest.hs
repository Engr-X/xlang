module Lexer.TokenizerTest where


import Lexer.Token
import Lexer.Tokenizer

import Test.Tasty
import Test.Tasty.HUnit

import Util.Types
import Util.Exception (ErrorKind, unclosedStrLiteralMsg, unclosedCharLiteralMsg, invalidCharLiteralMsg, invalidNumericLiteralMsg, unclosedCommentMsg)
import qualified Util.Exception as UE


name :: String
name = "Lexer.Tokenizer.tokenize"

path :: Path
path = "stdin"


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

makeLexErr :: String -> Int -> Int -> Int -> ErrorKind
makeLexErr why a b c = UE.Lexer $ UE.makeError path (makePosition a b c) why


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
        makeId "for" 1 1 3, makeSymbol LParen 1 5 1, makeId "x" 1 6 1, makeSymbol Assign 1 8 1, makeId "a_address" 1 10 9, makeSymbol Dot 1 19 1, makeId "getSize" 1 20 7, makeSymbol LParen 1 27 1, makeSymbol RParen 1 28 1, makeSymbol Semicolon 1 29 1,
            makeId "x" 1 31 1, makeSymbol LessEqual 1 33 2, makeNum "10" 1 36 2, makeSymbol Semicolon 1 38 1,
            makeId "x" 1 40 1, makeSymbol MultiplyAssign 1 42 2, makeNum "2" 1 45 1, makeSymbol RParen 1 46 1,

        makeId "print" 2 5 5, makeSymbol LParen 2 10 1, makeId "x" 2 11 1, makeSymbol RParen 2 12 1
    ])),


{-
0x1f + -42L * 3.14e-2f
-}
    ("0x1f + -42L * 3.14e-2f", ([], [
        makeNum "0x1f" 1 1 4, makeSymbol Plus 1 6 1, makeNum "-42L" 1 8 4, makeSymbol Multiply 1 13 1, makeNum "3.14e-2f" 1 15 8])),


{-
a<<=1; b>>=2; c!^=3; d**=4; e?->f
-}
    ("a<<=1; b>>=2; c!^=3; d**=4; e?->f", ([], [
        makeId "a" 1 1 1, makeSymbol BitLShiftAssign 1 2 3, makeNum "1" 1 5 1, makeSymbol Semicolon 1 6 1,
        makeId "b" 1 8 1, makeSymbol BitRShiftAssign 1 9 3, makeNum "2" 1 12 1, makeSymbol Semicolon 1 13 1,
        makeId "c" 1 15 1, makeSymbol BitXnorAssign 1 16 3, makeNum "3" 1 19 1, makeSymbol Semicolon 1 20 1,
        makeId "d" 1 22 1, makeSymbol PowerAssign 1 23 3, makeNum "4" 1 26 1, makeSymbol Semicolon 1 27 1,
        makeId "e" 1 29 1, makeSymbol QuestionArrow 1 30 3, makeId "f" 1 33 1])),


{-
a..b . c ++d -- e
-}
    ("a..b . c ++d -- e", ([], [
        makeId "a" 1 1 1, makeSymbol DoubleDot 1 2 2, makeId "b" 1 4 1, makeSymbol Dot 1 6 1, makeId "c" 1 8 1, makeSymbol PlusPlus 1 10 2,
            makeId "d" 1 12 1, makeSymbol MinusMinus 1 14 2, makeId "e" 1 17 1])),


{-
a=1/*x*/b=2//y
c=3
-}
    (unlines [
        "a=1/*x*/b=2//y",
        "c=3"], ([], [
        makeId "a" 1 1 1, makeSymbol Assign 1 2 1, makeNum "1" 1 3 1, makeId "b" 1 9 1, makeSymbol Assign 1 10 1, makeNum "2" 1 11 1,
        makeId "c" 2 1 1, makeSymbol Assign 2 2 1, makeNum "3" 2 3 1])),


{-
123abc
-}
    ("123abc", ([makeLexErr invalidNumericLiteralMsg 1 1 3], [makeId "abc" 1 4 3])),


{-
string a = "this is an string but I forget ..
char b = 'I love u guys
-}
    (unlines [
        "string a = \"this is an string but I forget ..",
        "char b = 'I love u guys"], (
        [makeLexErr unclosedStrLiteralMsg 1 12 34, makeLexErr unclosedCharLiteralMsg 2 10 14], 
        [makeId "string" 1 1 6, makeId "a" 1 8 1, makeSymbol Assign 1 10 1,
         makeId "char" 2 1 4, makeId "b" 2 6 1, makeSymbol Assign 2 8 1])),


{-
int a <<= 'ab' // this is a wrong expression
/* :) */ 
int y = 1e, z = 1e6
-}
    (unlines [
        "int a <<= 'ab' // this is a wrong expression",
        "/* :) */ ",
        "int y = 1e, z = 1e6"], (
        [makeLexErr invalidCharLiteralMsg 1 11 4, makeLexErr invalidNumericLiteralMsg 3 9 1],
        [makeId "int" 1 1 3, makeId "a" 1 5 1, makeSymbol BitLShiftAssign 1 7 3,
         makeId "int" 3 1 3, makeId "y" 3 5 1, makeSymbol Assign 3 7 1, makeId "e" 3 10 1, makeSymbol Comma 3 11 1, makeId "z" 3 13 1, makeSymbol Assign 3 15 1, makeNum "1e6" 3 17 3])),


{-
int main() {
  x=0; y=.5; z=1e6;
  s=\"hi\\n\"; c='\\t'; // end
  return x+y*z; }
-}
    (unlines [
        "int main() {",
        "  x=0; y=.5; z=1e6;",
        "  s=\"hi\\n\"; c='\\t'; // end",
        "  return x+y*z; }"], ([], [
            makeId "int" 1 1 3, makeId "main" 1 5 4, makeSymbol LParen 1 9 1, makeSymbol RParen 1 10 1, makeSymbol LBrace 1 12 1,

            makeId "x" 2 3 1,  makeSymbol Assign 2 4 1,  makeNum "0" 2 5 1,  makeSymbol Semicolon 2 6 1,
                makeId "y" 2 8 1,  makeSymbol Assign 2 9 1,  makeNum ".5" 2 10 2, makeSymbol Semicolon 2 12 1,
                makeId "z" 2 14 1, makeSymbol Assign 2 15 1, makeNum "1e6" 2 16 3, makeSymbol Semicolon 2 19 1,

            makeId "s" 3 3 1,  makeSymbol Assign 3 4 1,  makeStr "hi\\n" 3 5 6, makeSymbol Semicolon 3 11 1,
                makeId "c" 3 13 1, makeSymbol Assign 3 14 1, makeChar '\t' 3 15 4, makeSymbol Semicolon 3 19 1,

            makeId "return" 4 3 6, makeId "x" 4 10 1, makeSymbol Plus 4 11 1, makeId "y" 4 12 1, makeSymbol Multiply 4 13 1, makeId "z" 4 14 1,
                makeSymbol Semicolon 4 15 1, makeSymbol RBrace 4 17 1
        ])),


{-
arr[i++] += (x<<2) ** 3 - y--;
-}
    ("arr[i++] += (x<<2) ** 3 - y--;", ([], [
        makeId "arr" 1 1 3, makeSymbol LBracket 1 4 1, makeId "i" 1 5 1, makeSymbol PlusPlus 1 6 2, makeSymbol RBracket 1 8 1,
            makeSymbol PlusAssign 1 10 2, makeSymbol LParen 1 13 1, makeId "x" 1 14 1, makeSymbol BitLShift 1 15 2,
            makeNum "2" 1 17 1, makeSymbol RParen 1 18 1, makeSymbol Power 1 20 2, makeNum "3" 1 23 1, makeSymbol Minus 1 25 1,
            makeId "y" 1 27 1, makeSymbol MinusMinus 1 28 2, makeSymbol Semicolon 1 30 1
    ])),


{-
a=1; /* block
still
-}
    (unlines ["a=1; /* block", "still"], (
        [makeLexErr unclosedCommentMsg 1 6 15],
        [makeId "a" 1 1 1, makeSymbol Assign 1 2 1, makeNum "1" 1 3 1, makeSymbol Semicolon 1 4 1])),


{-
a = "this is unclosed
b = '\0'
c = '\t
/*

-}
    (unlines ["a = \"this is unclosed", "b = '\\0'", "c = '\\t", "/*"], (
        [makeLexErr unclosedStrLiteralMsg 1 5 17, makeLexErr unclosedCharLiteralMsg 3 5 3, makeLexErr unclosedCommentMsg 4 1 3],
        [makeId "a" 1 1 1, makeSymbol Assign 1 3 1,
         makeId "b" 2 1 1, makeSymbol Assign 2 3 1, makeChar '\0' 2 5 4,
         makeId "c" 3 1 1, makeSymbol Assign 3 3 1]))]



tests :: TestTree
tests = testGroup name $ zipWith (\idx (input, expect) -> makeTest idx input expect) [0..] datas

