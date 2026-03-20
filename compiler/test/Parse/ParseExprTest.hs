module Parse.ParseExprTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseExpr
import Util.Type
import Lex.Token (Token, Symbol)

import qualified Lex.Token as Lex


mkSym :: Symbol -> Int -> Int -> Int -> Token
mkSym s a b c = Lex.Symbol s $ Position a b c

mkNum :: String -> Int -> Int -> Int -> Token
mkNum s a b c = Lex.NumberConst s $ Position a b c

mkId :: String -> Int -> Int -> Int -> Token
mkId s a b c = Lex.Ident s $ makePosition a b c


lexparseExprTests :: TestTree
lexparseExprTests = testGroup "Parse.ParseExpr.lexparseExpr" $
    map (\(n, src, expected) -> testCase n $ replLexparseExpr src @=? expected) [
    ("0", "1 + 2 - 3", Right $
        Binary Sub
            (Binary Add
                (IntConst "1" $ mkNum "1" 1 1 1)
                (IntConst "2" $ mkNum "2" 1 5 1) $ mkSym Lex.Plus 1 3 1)
            (IntConst "3" $ mkNum "3" 1 9 1) $ mkSym Lex.Minus 1 7 1),

    ("1", "(1 + 2) * (3 - 4) / 5", Right $
        Binary Div
            (Binary Mul
                (Binary Add
                    (IntConst "1" $ mkNum "1" 1 2 1)
                    (IntConst "2" $ mkNum "2" 1 6 1)
                    (mkSym Lex.Plus 1 4 1))
                (Binary Sub
                    (IntConst "3" $ mkNum "3" 1 12 1)
                    (IntConst "4" $ mkNum "4" 1 16 1)
                    (mkSym Lex.Minus 1 14 1))
                (mkSym Lex.Multiply 1 9 1))
            (IntConst "5" $ mkNum "5" 1 21 1)
            (mkSym Lex.Divide 1 19 1)),

    ("1a", "10 % 3 * 2", Right $
        Binary Mul
            (Binary Mod
                (IntConst "10" $ mkNum "10" 1 1 2)
                (IntConst "3" $ mkNum "3" 1 6 1)
                (mkSym Lex.Modulo 1 4 1))
            (IntConst "2" $ mkNum "2" 1 10 1)
            (mkSym Lex.Multiply 1 8 1)),

    ("2", "-1 + +2 * -3", Right $ Binary Add
        (Unary UnaryMinus
            (IntConst "1" $ mkNum "1" 1 2 1)
            (mkSym Lex.Minus 1 1 1))
        (Binary Mul
            (Unary UnaryPlus
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 6 1))
            (Unary UnaryMinus
                (IntConst "3" $ mkNum "3" 1 12 1)
                (mkSym Lex.Minus 1 11 1))
            (mkSym Lex.Multiply 1 9 1))
        (mkSym Lex.Plus 1 4 1)),
        
    ("3", "a = b = c + 1", Right $
        Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Assign
                (Variable "b" $ mkId "b" 1 5 1)
                (Binary Add
                    (Variable "c" $ mkId "c" 1 9 1)
                    (IntConst "1" $ mkNum "1" 1 13 1)
                    (mkSym Lex.Plus 1 11 1))
                (mkSym Lex.Assign 1 7 1))
            (mkSym Lex.Assign 1 3 1)),

    ("3a", "x = x / i", Right $
        Binary Assign
            (Variable "x" $ mkId "x" 1 1 1)
            (Binary Div
                (Variable "x" $ mkId "x" 1 5 1)
                (Variable "i" $ mkId "i" 1 9 1)
                (mkSym Lex.Divide 1 7 1))
            (mkSym Lex.Assign 1 3 1)),

    ("3a1", "a -= 1", Right $
        Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Sub
                (Variable "a" $ mkId "a" 1 1 1)
                (IntConst "1" $ mkNum "1" 1 6 1)
                (mkSym Lex.MinusAssign 1 3 2))
            (mkSym Lex.MinusAssign 1 3 2)),

    ("3a2", "a *= 2", Right $
        Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Mul
                (Variable "a" $ mkId "a" 1 1 1)
                (IntConst "2" $ mkNum "2" 1 6 1)
                (mkSym Lex.MultiplyAssign 1 3 2))
            (mkSym Lex.MultiplyAssign 1 3 2)),

    ("3a3", "a /= 3", Right $
        Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Div
                (Variable "a" $ mkId "a" 1 1 1)
                (IntConst "3" $ mkNum "3" 1 6 1)
                (mkSym Lex.DivideAssign 1 3 2))
            (mkSym Lex.DivideAssign 1 3 2)),

    ("3a4", "a %= 4", Right $
        Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Mod
                (Variable "a" $ mkId "a" 1 1 1)
                (IntConst "4" $ mkNum "4" 1 6 1)
                (mkSym Lex.ModuloAssign 1 3 2))
            (mkSym Lex.ModuloAssign 1 3 2)),

    ("3a5", "a **= 5", Right $
        Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Pow
                (Variable "a" $ mkId "a" 1 1 1)
                (IntConst "5" $ mkNum "5" 1 7 1)
                (mkSym Lex.PowerAssign 1 3 3))
            (mkSym Lex.PowerAssign 1 3 3)),

    ("3b", "x % i == 0", Right $
        Binary Equal
            (Binary Mod
                (Variable "x" $ mkId "x" 1 1 1)
                (Variable "i" $ mkId "i" 1 5 1)
                (mkSym Lex.Modulo 1 3 1))
            (IntConst "0" $ mkNum "0" 1 10 1)
            (mkSym Lex.Equal 1 7 2)),
            
    ("4", "a + 1 > b * 2 + (c - 3)", Right $
        Binary GreaterThan
            (Binary Add
                (Variable "a" $ mkId "a" 1 1 1)
                (IntConst "1" $ mkNum "1" 1 5 1)
                (mkSym Lex.Plus 1 3 1))
            (Binary Add
                (Binary Mul
                    (Variable "b" $ mkId "b" 1 9 1)
                    (IntConst "2" $ mkNum "2" 1 13 1)
                    (mkSym Lex.Multiply 1 11 1))
                (Binary Sub
                    (Variable "c" $ mkId "c" 1 18 1)
                    (IntConst "3" $ mkNum "3" 1 22 1)
                    (mkSym Lex.Minus 1 20 1))
                (mkSym Lex.Plus 1 15 1))
            (mkSym Lex.GreaterThan 1 7 1)),
            
    ("5", "(x * 2 + 3) < y - 4 / 2", Right $
        Binary LessThan
            (Binary Add
                (Binary Mul
                    (Variable "x" $ mkId "x" 1 2 1)
                    (IntConst "2" $ mkNum "2" 1 6 1)
                    (mkSym Lex.Multiply 1 4 1))
                (IntConst "3" $ mkNum "3" 1 10 1)
                (mkSym Lex.Plus 1 8 1))
            (Binary Sub
                (Variable "y" $ mkId "y" 1 15 1)
                (Binary Div
                    (IntConst "4" $ mkNum "4" 1 19 1)
                    (IntConst "2" $ mkNum "2" 1 23 1)
                    (mkSym Lex.Divide 1 21 1))
                (mkSym Lex.Minus 1 17 1))
            (mkSym Lex.LessThan 1 13 1)),
            
    ("6", "a + b == c * (d - e) + 1", Right $
        Binary Equal
            (Binary Add
                (Variable "a" $ mkId "a" 1 1 1)
                (Variable "b" $ mkId "b" 1 5 1)
                (mkSym Lex.Plus 1 3 1))
            (Binary Add
                (Binary Mul
                    (Variable "c" $ mkId "c" 1 10 1)
                    (Binary Sub
                        (Variable "d" $ mkId "d" 1 15 1)
                        (Variable "e" $ mkId "e" 1 19 1)
                        (mkSym Lex.Minus 1 17 1))
                    (mkSym Lex.Multiply 1 12 1))
                (IntConst "1" $ mkNum "1" 1 24 1)
                (mkSym Lex.Plus 1 22 1))
            (mkSym Lex.Equal 1 7 2)),
            
    ("7", "(m + 1) != n / 2 - (p + q)", Right $
        Binary NotEqual
            (Binary Add
                (Variable "m" $ mkId "m" 1 2 1)
                (IntConst "1" $ mkNum "1" 1 6 1)
                (mkSym Lex.Plus 1 4 1))
            (Binary Sub
                (Binary Div
                    (Variable "n" $ mkId "n" 1 12 1)
                    (IntConst "2" $ mkNum "2" 1 16 1)
                    (mkSym Lex.Divide 1 14 1))
                (Binary Add
                    (Variable "p" $ mkId "p" 1 21 1)
                    (Variable "q" $ mkId "q" 1 25 1)
                    (mkSym Lex.Plus 1 23 1))
                (mkSym Lex.Minus 1 18 1))
            (mkSym Lex.NotEqual 1 9 2)),

    ("7a", "a >= b + 1", Right $
        Binary GreaterEqual
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Add
                (Variable "b" $ mkId "b" 1 6 1)
                (IntConst "1" $ mkNum "1" 1 10 1)
                (mkSym Lex.Plus 1 8 1))
            (mkSym Lex.GreaterEqual 1 3 2)),

    ("7b", "x <= y * 2", Right $
        Binary LessEqual
            (Variable "x" $ mkId "x" 1 1 1)
            (Binary Mul
                (Variable "y" $ mkId "y" 1 6 1)
                (IntConst "2" $ mkNum "2" 1 10 1)
                (mkSym Lex.Multiply 1 8 1))
            (mkSym Lex.LessEqual 1 3 2)),
            
    ("8", "!a > b", Right $
        Binary GreaterThan
            (Unary LogicalNot
                (Variable "a" $ mkId "a" 1 2 1)
                (mkSym Lex.LogicalNot 1 1 1))
            (Variable "b" $ mkId "b" 1 6 1)
            (mkSym Lex.GreaterThan 1 4 1)),

    ("9", "!a == b", Right $
        Binary Equal
            (Unary LogicalNot
                (Variable "a" $ mkId "a" 1 2 1)
                (mkSym Lex.LogicalNot 1 1 1))
            (Variable "b" $ mkId "b" 1 7 1)
            (mkSym Lex.Equal 1 4 2)),

    ("10", "!!a != !b", Right $
        Binary NotEqual
            (Unary LogicalNot
                (Unary LogicalNot
                    (Variable "a" $ mkId "a" 1 3 1)
                    (mkSym Lex.LogicalNot 1 2 1))
                (mkSym Lex.LogicalNot 1 1 1))
            (Unary LogicalNot
                (Variable "b" $ mkId "b" 1 9 1)
                (mkSym Lex.LogicalNot 1 8 1))
            (mkSym Lex.NotEqual 1 5 2)),

    ("11", "!(a < b) == !c", Right $
        Binary Equal
            (Unary LogicalNot
                (Binary LessThan
                    (Variable "a" $ mkId "a" 1 3 1)
                    (Variable "b" $ mkId "b" 1 7 1)
                    (mkSym Lex.LessThan 1 5 1))
                (mkSym Lex.LogicalNot 1 1 1))
            (Unary LogicalNot
                (Variable "c" $ mkId "c" 1 14 1)
                (mkSym Lex.LogicalNot 1 13 1))
            (mkSym Lex.Equal 1 10 2)),
            
    ("12", "addToMap::<int>(m, 1)", Right $
            CallT
                (Variable "addToMap" (mkId "addToMap" 1 1 8))
                [(Int32T, [mkId "int" 1 12 3])]
                [Variable "m" (mkId "m" 1 17 1), IntConst "1" (mkNum "1" 1 20 1)]
        ),
        
    ("13", "makePair::<int, double>(x, 2)", Right $
        CallT
            (Variable "makePair" (mkId "makePair" 1 1 8))
            [
                (Int32T, [mkId "int" 1 12 3]),
                (Float64T, [mkId "double" 1 17 6])
            ] [
                Variable "x" (mkId "x" 1 25 1),
                IntConst "2" (mkNum "2" 1 28 1)]
        ),

    ("14", "foo::<HashMap::<List::<Int>>>(h)", Right $
            CallT
                (Variable "foo" (mkId "foo" 1 1 3))
                [(
                    Class ["HashMap"] [Class ["List"] [Class ["Int"] []]], [
                    mkId "HashMap" 1 7 7,
                    mkSym Lex.DoubleColon 1 14  2,
                    mkSym Lex.LessThan    1 16  1,
                    mkId  "List"          1 17  4,
                    mkSym Lex.DoubleColon 1 21  2,
                    mkSym Lex.LessThan    1 23  1,
                    mkId  "Int"           1 24  3,
                    mkSym Lex.GreaterThan 1 27  1,
                    mkSym Lex.GreaterThan 1 28  1])
                ]
                [Variable "h" (mkId "h" 1 31 1)]
        ),

    ("15", "bar::<List::<int>, HashMap::<int>>(a, b)", Right $
        CallT
            (Variable "bar" (mkId "bar" 1 1 3))
                [(
                    Class ["List"] [Int32T], [
                    mkId "List" 1 7 4,
                    mkSym Lex.DoubleColon 1 11 2,
                    mkSym Lex.LessThan 1 13 1,
                    mkId "int" 1 14 3,
                    mkSym Lex.GreaterThan 1 17 1]), (
                        
                        Class ["HashMap"] [Int32T], [
                            mkId "HashMap" 1 20  7,
                            mkSym Lex.DoubleColon 1 27 2,
                            mkSym Lex.LessThan 1 29 1,
                            mkId  "int" 1 30 3,
                            mkSym Lex.GreaterThan 1 33 1])
                    ] [
                        Variable "a" (mkId "a" 1 36 1),
                        Variable "b" (mkId "b" 1 39 1)]
        ),

    ("16", "size::<HashMap::<List::<Int>>>(h) + 1", Right $
        Binary Add
            (CallT
                (Variable "size" (mkId "size" 1 1 4))
                [(
                    Class ["HashMap"] [Class ["List"] [Class ["Int"] []]], [
                        mkId  "HashMap" 1 8 7,
                        mkSym Lex.DoubleColon 1 15 2,
                        mkSym Lex.LessThan 1 17 1,
                        mkId  "List" 1 18 4,
                        mkSym Lex.DoubleColon 1 22 2,
                        mkSym Lex.LessThan 1 24 1,
                        mkId  "Int" 1 25 3,
                        mkSym Lex.GreaterThan 1 28 1,
                        mkSym Lex.GreaterThan 1 29 1])
                    ]
                    [Variable "h" (mkId "h" 1 32 1)]
                )
                (IntConst "1" (mkNum "1" 1 37 1))
                (mkSym Lex.Plus 1 35 1)
    )]

tests :: TestTree
tests = testGroup "Parse.ParseExpr" [lexparseExprTests, ternaryParseTests, incDecParseTests, andOrParseTests]


ternaryParseTests :: TestTree
ternaryParseTests = testGroup "Parse.ParseExpr.ternary" [
    testCase "ternary simple" $
        case replLexparseExpr "a if b else c" of
            Right (Ternary (Variable "b" _) (Variable "a" _, Variable "c" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "ternary in assignment rhs" $
        case replLexparseExpr "x = a if b else c" of
            Right (Binary Assign (Variable "x" _) (Ternary (Variable "b" _) (Variable "a" _, Variable "c" _) _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "ternary precedence below add" $
        case replLexparseExpr "a + 1 if b else c + 2" of
            Right (Ternary
                (Variable "b" _)
                (Binary Add (Variable "a" _) (IntConst "1" _) _, Binary Add (Variable "c" _) (IntConst "2" _) _)
                _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other)
    ]


incDecParseTests :: TestTree
incDecParseTests = testGroup "Parse.ParseExpr.incDec" [
    testCase "prefix ++ parses as IncSelf" $
        case replLexparseExpr "++a" of
            Right (Unary IncSelf (Variable "a" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "prefix -- parses as DecSelf" $
        case replLexparseExpr "--a" of
            Right (Unary DecSelf (Variable "a" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "postfix ++ parses as SelfInc" $
        case replLexparseExpr "a++" of
            Right (Unary SelfInc (Variable "a" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "postfix -- parses as SelfDec" $
        case replLexparseExpr "a--" of
            Right (Unary SelfDec (Variable "a" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "a++ + 1 keeps postfix on lhs" $
        case replLexparseExpr "a++ + 1" of
            Right (Binary Add (Unary SelfInc (Variable "a" _) _) (IntConst "1" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "++a + 1 keeps prefix on lhs" $
        case replLexparseExpr "++a + 1" of
            Right (Binary Add (Unary IncSelf (Variable "a" _) _) (IntConst "1" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "postfix chain after paren parses" $
        case replLexparseExpr "(a)++" of
            Right (Unary SelfInc (Variable "a" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "postfix before cast parses in Postfix layer" $
        case replLexparseExpr "a++ as int" of
            Right (Cast (Int32T, _) (Unary SelfInc (Variable "a" _) _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other)
    ]


andOrParseTests :: TestTree
andOrParseTests = testGroup "Parse.ParseExpr.andOr" [
    testCase "and has higher precedence than or" $
        case replLexparseExpr "1 or 2 and 3" of
            Right (Binary BitOr (IntConst "1" _) (Binary BitAnd (IntConst "2" _) (IntConst "3" _) _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other),

    testCase "left associativity for or" $
        case replLexparseExpr "a or b or c" of
            Right (Binary BitOr (Binary BitOr (Variable "a" _) (Variable "b" _) _) (Variable "c" _) _) -> pure ()
            other -> assertFailure ("unexpected parse result: " ++ show other)
    ]

