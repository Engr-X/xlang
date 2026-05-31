module Parse.SyntaxTreeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Util.Type
import Util.Basic
import Data.Char (toLower)
import Data.List (isPrefixOf, find)
import Lex.Token (Token, Symbol)

import qualified Lex.Token as Lex
import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet


pos :: Position
pos = makePosition 0 0 0

mkNumD :: String -> Token
mkNumD s = Lex.NumberConst s $ makePosition 0 0 0

mkIdD :: String -> Token
mkIdD s = Lex.Ident s $ makePosition 0 0 0

mkSymD :: Symbol -> Token
mkSymD s = Lex.Symbol s $ makePosition 0 0 0


flattenExprTests :: TestTree
flattenExprTests = testGroup "Parse.SyntaxTree.flattenExpr" $ map (\(i, inp, out) ->
    testCase i $ flattenExpr inp @=? out) [
        ("0", Nothing, []),
        ("1", Just (IntConst "1" (mkNumD "1")), [IntConst "1" (mkNumD "1")]),
        ("2", Just (Binary Mul
            (IntConst "2" (mkNumD "2"))
            (IntConst "3" (mkNumD "3"))
            (mkSymD Lex.Multiply)), [
                Binary Mul
                (IntConst "2" (mkNumD "2"))
                (IntConst "3" (mkNumD "3"))
                (mkSymD Lex.Multiply), IntConst "2" (mkNumD "2"), IntConst "3" (mkNumD "3")]),
        
        ("3",
            Just (Call
                (Qualified ["f"] [])
                [
                    IntConst "1" (mkNumD "1"),
                    Call
                        (Qualified ["g"] [])
                        [IntConst "2" (mkNumD "2")]
                ]), [
                Call
                    (Qualified ["f"] [])
                    [
                        IntConst "1" (mkNumD "1"),
                        Call
                            (Qualified ["g"] [])
                            [IntConst "2" (mkNumD "2")]
                    ],
                Qualified ["f"] [],
                IntConst "1" (mkNumD "1"),
                Call
                    (Qualified ["g"] [])
                    [IntConst "2" (mkNumD "2")],
                Qualified ["g"] [],
                IntConst "2" (mkNumD "2")
            ]
        )
    ]


flattenBlockTests :: TestTree
flattenBlockTests = testGroup "Parse.SyntaxTree.flattenBlock" $ map (\(i, inp, out) -> testCase i $ flattenBlock inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Multiple
        [Expr (IntConst "1" $ mkNumD "1"), Expr (IntConst "2" $ mkNumD "2")]),
        [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")]),

    ("2", Just (Multiple
        [Expr (IntConst "1" $ mkNumD "1"), Expr (BoolConst True $ mkIdD "true"), Expr (Variable "x" $ mkIdD "x")]),
        [IntConst "1" $ mkNumD "1", BoolConst True $ mkIdD "true", Variable "x" $ mkIdD "x"]),

    ("3", Just (Multiple [Expr (IntConst "1" $ mkNumD "1"), Expr (Binary Mul
            (IntConst "2" $ mkNumD "2")
            (IntConst "3" $ mkNumD "3")
            (mkSymD Lex.Multiply))]),
        [IntConst "1" $ mkNumD "1",
            Binary Mul
                (IntConst "2" $ mkNumD "2")
                (IntConst "3" $ mkNumD "3")
                (mkSymD Lex.Multiply),
            IntConst "2" $ mkNumD "2", IntConst "3" $ mkNumD "3"])]


flattenCaseTests :: TestTree
flattenCaseTests = testGroup "Parse.SyntaxTree.flattenCase" $ map (\(i, inp, out) -> testCase i $ flattenCase inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Case (IntConst "1" $ mkNumD "1") (Just $ Multiple [Expr (IntConst "2" $ mkNumD "2")]) dummyTok),
        [IntConst "1" $ mkNumD "1", IntConst "2" $ mkNumD "2"]),

    ("2", Just (Case (IntConst "1" $ mkNumD "1") (Just $ Multiple [Expr (IntConst "2" $ mkNumD "2"), Expr (IntConst "3" $ mkNumD "3")]) dummyTok),
        [IntConst "1" $ mkNumD "1", IntConst "2" $ mkNumD "2", IntConst "3" $ mkNumD "3"]),

    ("3", Just (Case (Binary Add (IntConst "1" $ mkNumD "1") (IntConst "2" $ mkNumD "2") (mkSymD Lex.Plus)) (Just $ Multiple [Expr (IntConst "3" $ mkNumD "3")]) dummyTok),
        [Binary Add (IntConst "1" $ mkNumD "1") (IntConst "2" $ mkNumD "2") (mkSymD Lex.Plus), IntConst "3" $ mkNumD "3"]),

    ("4", Just (Default (Multiple [Expr (IntConst "1" $ mkNumD "1")]) dummyTok), [IntConst "1" $ mkNumD "1"]),

    ("5", Just (Default (Multiple [Expr (IntConst "1" $ mkNumD "1"), Expr (BoolConst True $ mkIdD "true")]) dummyTok),
        [IntConst "1" $ mkNumD "1", BoolConst True $ mkIdD "true"]),

    ("6", Just (Case (IntConst "1" $ mkNumD "1") (Just $ Multiple []) dummyTok), [IntConst "1" $ mkNumD "1"]),

    ("7", Just (Default (Multiple []) dummyTok), [])]
    where
        dummyTok :: Token
        dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)



flattenStatementTests :: TestTree
flattenStatementTests = testGroup "Parse.SyntaxTree.flattenStatement" $ map (\(i, inp, out) -> testCase i $ flattenStatement inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Expr (IntConst "1" $ mkNumD "1")), [IntConst "1" $ mkNumD "1"]),

    ("2", Just (BlockStmt (Multiple [
        If (BoolConst True $ mkIdD "true")
        (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")]))
        (Just (Multiple [Expr (IntConst "0" $ mkNumD "0")]))
        (dummyTok, Just dummyTok)
        ])), [BoolConst True $ mkIdD "true", IntConst "1" $ mkNumD "1", IntConst "0" $ mkNumD "0"]),
            
    ("3", Just (
        If (BoolConst True $ mkIdD "true") (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")])) Nothing (dummyTok, Nothing)), [
        BoolConst True $ mkIdD "true", IntConst "1" $ mkNumD "1"]),

    ("4", Just (For (Just (Expr (IntConst "1" $ mkNumD "1")), Just (IntConst "2" $ mkNumD "2"), Just (Expr (IntConst "3" $ mkNumD "3")))
        (Just (Multiple [Expr (IntConst "4" $ mkNumD "4")])) Nothing (dummyTok, Nothing)),
        [IntConst "1" $ mkNumD "1", IntConst "2" $ mkNumD "2", IntConst "3" $ mkNumD "3", IntConst "4" $ mkNumD "4"]),

    ("5", Just (For (Nothing, Just (IntConst "2" $ mkNumD "2"), Nothing) Nothing Nothing (dummyTok, Nothing)), [IntConst "2" $ mkNumD "2"]),

    ("6", Just (While (Variable "cond" $ mkIdD "cond") (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")])) Nothing (dummyTok, Nothing)),
        [Variable "cond" $ mkIdD "cond", IntConst "1" $ mkNumD "1"]),

    ("7", Just (Switch (Variable "x" $ mkIdD "x") [
        Case (IntConst "1" $ mkNumD "1") (Just $ Multiple [Expr (IntConst "10" $ mkNumD "10")]) dummyTok,
        Default (Multiple [Expr (IntConst "0" $ mkNumD "0")]) dummyTok] dummyTok),
        [Variable "x" $ mkIdD "x", IntConst "1" $ mkNumD "1", IntConst "10" $ mkNumD "10", IntConst "0" $ mkNumD "0"])]
    where
        dummyTok :: Token
        dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


-- Dummy tokens for declarations. Predicates only check constructors.
dummyToks :: [Token]
dummyToks = []


isPackageDeclTests :: TestTree
isPackageDeclTests = testGroup "Parse.SyntaxTree.isPackageDecl" $
    map (\(i, decl, out) -> testCase i $ isPackageDecl decl @=? out) [
        ("0", Package ["a"] dummyToks, True),
        ("1", Package ["com","wangdi"] dummyToks, True),
        ("2", Import ["a"] dummyToks, False),
        ("3", Import ["x","y"] dummyToks, False)
    ]


isImportDeclTests :: TestTree
isImportDeclTests = testGroup "Parse.SyntaxTree.isImportDecl" $ map (\(i, decl, out) ->
    testCase i $ isImportDecl decl @=? out) [
        ("0", Import ["a"] dummyToks, True),
        ("1", Import ["com","wangdi","IO"] dummyToks, True),
        ("2", Package ["a"] dummyToks, False),
        ("3", Package ["x","y"] dummyToks, False)]


isClassDeclarTests :: TestTree
isClassDeclarTests = testGroup "Parse.SyntaxTree.isClassDeclar" $ map (\(i, stmt, out) ->
    testCase i $ isClassDeclar stmt @=? out) [
        ("0", Expr (IntConst "1" $ mkNumD "1"), False),
        ("1", Command (Return Nothing) (mkIdD "return"), False)
    ]


isFunctionTests :: TestTree
isFunctionTests = testGroup "Parse.SyntaxTree.isFunction" $ map (\(i, stmt, out) ->
    testCase i $ isFunction stmt @=? out) [
        ("0", Function (Int32T, [mkIdD "int"]) (Variable "f" $ mkIdD "f") [] (Multiple []), True),
        ("1", Function (Int32T, [mkIdD "int"]) (Variable "g" $ mkIdD "g")
            [(Int32T, "x", [mkIdD "x"])] (Multiple []), True),
        ("2", FunctionT (Int32T, [mkIdD "int"]) (Variable "f" $ mkIdD "f")
            [(Int32T, [mkIdD "T"])] [] (Multiple []), False),
        ("3", Expr (IntConst "1" $ mkNumD "1"), False)
    ]


isFunctionTTests :: TestTree
isFunctionTTests = testGroup "Parse.SyntaxTree.isFunctionT" $ map (\(i, stmt, out) ->
    testCase i $ isFunctionT stmt @=? out) [
        ("0", FunctionT (Int32T, [mkIdD "int"]) (Variable "f" $ mkIdD "f")
            [(Int32T, [mkIdD "T"])] [] (Multiple []), True),
        ("1", FunctionT (Int32T, [mkIdD "int"]) (Variable "g" $ mkIdD "g")
            [(Int32T, [mkIdD "T"])] [(Int32T, "x", [mkIdD "x"])] (Multiple []), True),
        ("2", Function (Int32T, [mkIdD "int"]) (Variable "f" $ mkIdD "f") [] (Multiple []), False),
        ("3", Expr (IntConst "1" $ mkNumD "1"), False)
    ]


isAssignmentTests :: TestTree
isAssignmentTests = testGroup "Parse.SyntaxTree.isAssignment" $ map (\(i, stmt, out) ->
    testCase i $ isAssignment stmt @=? out) [
        ("0", Expr (Binary Assign (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.Assign)), True),
        ("1", Expr (Binary PlusAssign (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.PlusAssign)), True),
        ("2", Expr (Binary MinusAssign (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.MinusAssign)), True),
        ("3", Expr (Binary MultiplyAssign (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.MultiplyAssign)), True),
        ("4", Expr (Binary DivideAssign (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.DivideAssign)), True),
        ("5", Expr (Binary ModuloAssign (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.ModuloAssign)), True),
        ("6", Expr (Binary PowerAssign (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.PowerAssign)), True),
        ("7", Expr (Binary Add (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.Plus)), False),
        ("8", Command (Return Nothing) (mkIdD "return"), False)
    ]


declPathTests :: TestTree
declPathTests = testGroup "Parse.SyntaxTree.declPath" $
    map (\(i, decl, out) -> testCase i $ declPath decl @=? out) [
        ("0", Package ["a"] [], ["a"]),
        ("1", Package ["java", "lang", "util"] [], ["java", "lang", "util"]),
        ("2", Import ["math"] [], ["math"]),
        ("3", Import ["java", "util", "List"] [], ["java", "util", "List"]),
        ("4", JavaName "Utils" (mkIdD "Utils"), [])]


getPackageTests :: TestTree
getPackageTests = testGroup "Parse.SyntaxTree.getPackage" $
    map (\(i, prog, out) -> testCase i $ getPackage prog @=? out) [
        ("0", ([], []), []),
        ("1", ([Package ["com", "wangdi"] []], []), ["com", "wangdi"]),
        ("2", ([Import ["java", "util", "List"] []], []), []),
        ("3",
            ([
                Package ["a", "b"] [],
                Import ["x", "y"] [],
                Package ["c", "d"] []
             ], []),
            ["a", "b"])
        ]


collectInputProgramTests :: TestTree
collectInputProgramTests = testGroup "Parse.SyntaxTree.collectInputProgram" $
    map (\(i, prog, out) -> testCase i $ collectInputProgram prog @=? out) [
        ("0", ([], []), Map.empty),
        ("1",
            ([Import ["java", "util", "List"] []], []),
            Map.fromList [(["java", "util"], HashSet.fromList ["List"])]),
        ("2",
            ([Import ["java", "util", "List"] [],
              Import ["java", "util", "Map"] [],
              Import ["java", "util", "List"] []], []),
            Map.fromList [(["java", "util"], HashSet.fromList ["List", "Map"])]),
        ("3",
            ([Import ["java", "lang", "String"] [],
              Import ["java", "lang", "*"] []], []),
            Map.fromList [(["java", "lang"], HashSet.fromList ["*"])])
    ]


collectInputProgramsTests :: TestTree
collectInputProgramsTests = testGroup "Parse.SyntaxTree.collectInputPrograms" $
    map (\(i, progs, out) -> testCase i $ collectInputPrograms progs @=? out) [
        ("0", [], Map.empty),
        ("1",
            [([Import ["java", "util", "List"] []], [])],
            Map.fromList [(["java", "util"], HashSet.fromList ["List"])]),
        ("2",
            [([Import ["java", "util", "List"] []], []),
             ([Import ["java", "lang", "String"] []], [])],
            Map.fromList [
                (["java", "util"], HashSet.fromList ["List"]),
                (["java", "lang"], HashSet.fromList ["String"])
            ]),
        ("3",
            [([Import ["java", "util", "List"] []], []),
             ([Import ["java", "util", "Set"] []], [])],
            Map.fromList [(["java", "util"], HashSet.fromList ["List", "Set"])])
    ]


flattenProgramTests :: TestTree
flattenProgramTests = testGroup "Parse.SyntaxTree.flattenProgram" $ map (\(i, inp, out) -> testCase i $ flattenProgram inp @=? out) [
    ("0", ([], []), []),
    ("1", ([], [Expr (IntConst "1" $ mkNumD "1")]), [IntConst "1" $ mkNumD "1"]),
    ("2", ([], [Expr (IntConst "1" $ mkNumD "1"), Expr (BoolConst True $ mkIdD "true")]), [IntConst "1" $ mkNumD "1", BoolConst True $ mkIdD "true"]),
    ("3", ([], [If (BoolConst True $ mkIdD "true") (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")])) Nothing (dummyTok, Nothing)]), [BoolConst True $ mkIdD "true", IntConst "1" $ mkNumD "1"])]
    where
        dummyTok :: Token
        dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


getErrorProgramTests :: TestTree
getErrorProgramTests = testGroup "Parse.SyntaxTree.getErrorProgram" $ map (\(i, inp, out) -> testCase i $ getErrorProgram inp @=? out) [
    ("0", ([], []), []),
    ("1", ([], [Expr (IntConst "1" $ mkNumD "1")]), []),
    ("2", ([], [Expr (makeError "err")]), [makeError "err"]),
    ("3", ([], [ If (BoolConst True $ mkIdD "true")
        (Just (Multiple [Expr (makeError "err1")]))
        (Just (Multiple [Expr (makeError "err2")]))
        (dummyTok "if", Just (dummyTok "else"))]),
        [ makeError "err1", makeError "err2"])]
    where
        dummyTok :: String -> Token
        dummyTok str = Lex.Ident str (makePosition 1 1 (length str))

        makeError :: String -> Expression
        makeError s = Error [dummyTok s] "why?"


normalizeClassTests :: TestTree
normalizeClassTests = testGroup "Parse.SyntaxTree.normalizeClass" $
    map (\(name, input, expected) -> testCase name $ normalizeClass input @?= expected) [
        ("0", Class ["int"] [], Int32T),
        ("1", Class ["List"] [Class ["int"] []], Class ["List"] [Int32T]),
        ("2", Class ["List"] [Class ["int"] []], Class ["List"] [Int32T]),
        ("3", Class ["int"] [Class ["int"] []], Class ["int"] [Int32T]),
        ("4", Class ["String"] [], Class ["String"] []),
        ("5", Class ["string"] [], Class ["string"] []),
        ("6", Class ["pointer"] [Class ["int"] []], Pointer Int32T),
        ("7", Class ["pointer"] [Class ["pointer"] [Class ["int"] []]], Pointer (Pointer Int32T)),
        ("8", Class ["pointer"] [Class ["void"] []], Pointer Void),
        ("9", Class ["pointer"] [Class ["*"] []], Pointer Void)]


classMangleDemangleTests :: TestTree
classMangleDemangleTests = testGroup "Parse.SyntaxTree.classMangleDemangle" $
    map (\(name, cls) -> testCase name $ classDemangle (classMangle cls) @?= cls) [
        ("0", Int32T),
        ("1", Class ["Pair"] []),
        ("2", Class ["List"] [Int32T]),
        ("3", Class ["com", "wangdi", "Pair"] []),
        ("4", Class ["com", "wangdi", "Map"] [Class ["String"] [], Class ["com", "wangdi", "Pair"] [Int64T]]),
        ("5", Pointer Int32T),
        ("6", Pointer (Pointer (Class ["Person"] [])))
    ]


{-toClassTests :: TestTree
toClassTests = testGroup "Parse.SyntaxTree.toClass" $ map (\(i, inp, out) -> testCase i $ toClass inp @=? out) [
    ("0", ["bool"], Bool), ("1",  ["int"], Int32T), ("2", ["int32"], Int32T), ("3", ["int64"], Int64T), ("4", ["float"], Float32T),
    ("5", ["double"], Float64T), ("6", ["float64"], Float64T), ("7", ["byte"], Int8T), ("8", ["short"], Int16T),
    ("9", ["MyClass"], Class ["MyClass"]), ("10", ["com", "wangdi", "Math"], Class ["com", "wangdi", "Math"]), ("11", ["java", "lang", "String"], Class ["java", "lang", "String"])] -}


isVariableTests :: TestTree
isVariableTests = testGroup "Parse.SyntaxTree.isVariable" $ map (\(i, inp, out) -> testCase i $ isVariable inp @=? out) [
    ("0", Variable "a" dummyTok, True),
    ("1", Qualified ["java","lang","Math"] [dummyTok], True),
    ("2", IntConst "1" dummyTok, False),
    ("3", Binary Add (Variable "a" dummyTok) (IntConst "1" dummyTok) dummyTok, False)]
        where
            dummyTok :: Token
            dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


isAtomTests :: TestTree
isAtomTests = testGroup "Parse.SyntaxTree.isAtom" $ map (\(i, inp, out) -> testCase i $ isAtom inp @=? out) [
    ("0", IntConst "1" dummyTok, True),
    ("1", Qualified ["java","lang","Math"] [dummyTok], True),
    ("2", Unary UnaryMinus (IntConst "1" dummyTok) dummyTok, False),
    ("3", Call (Variable "f" dummyTok) [IntConst "1" dummyTok], False)]
        where
            dummyTok :: Token
            dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


identTextTests :: TestTree
identTextTests = testGroup "Parse.ParserBasic.identText" $ map (\(i, inp, out) -> testCase i $ identText inp @=? out) [
    ("0", Lex.Ident "a" (makePosition 1 1 1), "a"),
    ("1", Lex.Ident "abc" (makePosition 1 1 3), "abc"),
    ("2", Lex.Ident "my_var" (makePosition 1 1 6), "my_var"),
    ("3", Lex.Ident "String" (makePosition 1 1 6), "String")]


numTextTests :: TestTree
numTextTests = testGroup "Parse.ParserBasic.numText" $ map (\(i, inp, out) -> testCase i $ numText inp @=? out) [
    ("0", Lex.NumberConst "1" (makePosition 1 1 1), "1"),
    ("1", Lex.NumberConst "123" (makePosition 1 1 3), "123"),
    ("2", Lex.NumberConst "-42" (makePosition 1 1 3), "-42"),
    ("3", Lex.NumberConst "3.14" (makePosition 1 1 4), "3.14")]


charValTests :: TestTree
charValTests = testGroup "Parse.ParserBasic.charVal" $ map (\(i, inp, out) -> testCase i $ charVal inp @=? out) [
    ("0", Lex.CharConst 'a' (makePosition 1 1 1), 'a'),
    ("1", Lex.CharConst '7' (makePosition 1 1 1), '7'),
    ("2", Lex.CharConst '_' (makePosition 1 1 1), '_'),
    ("3", Lex.CharConst ' ' (makePosition 1 1 1), ' ')]


strValTests :: TestTree
strValTests = testGroup "Parse.ParserBasic.strVal" $ map (\(i, inp, out) -> testCase i $ strVal inp @=? out) [
    ("0", Lex.StrConst "" (makePosition 1 1 0), ""),
    ("1", Lex.StrConst "hello" (makePosition 1 1 5), "hello"),
    ("2", Lex.StrConst "hello world" (makePosition 1 1 11), "hello world"),
    ("3", Lex.StrConst "a+b*c" (makePosition 1 1 5), "a+b*c")]


exprTokensTests :: TestTree
exprTokensTests = testGroup "Parse.SyntaxTree.exprTokens" $ map (\(n, e, ts) -> testCase n $ exprTokens e @=? ts) [
    ("0", Variable "x" tokX, [tokX]),
    ("1", Qualified ["java","lang","String"] [tokJava, tokLang, tokString], [tokJava, tokLang, tokString]),
    ("2", Unary UnaryMinus (IntConst "1" tok1) tokMinus, [tokMinus, tok1]),
    ("3", Binary Add (Variable "a" tokA) (Variable "b" tokB) tokPlus, [tokPlus, tokA, tokB]),
    ("4", Error [tokErr1, tokErr2] "bad", [tokErr1, tokErr2]),
    ("5", Cast ((Int32T, [tokCastTy])) (Variable "x" tokX) tokCast, [tokX, tokCast, tokCastTy]),
    ("6", Call (Variable "f" tokF) [Variable "x" tokX, IntConst "1" tok1], [tokF, tokX, tok1]),
    ("7", CallT (Variable "id" tokId) [(Int32T, [tokTA1]), (Bool, [tokTA2])] [Variable "x" tokX], [tokTA1, tokTA2, tokId, tokX])]
    where
        tokX, tokJava, tokLang, tokString, tok1, tokMinus, tokA, tokB, tokPlus :: Token
        tokX      = Lex.Ident "x"      (makePosition 1 1 1)
        tokJava   = Lex.Ident "java"   (makePosition 1 1 4)
        tokLang   = Lex.Ident "lang"   (makePosition 1 6 4)
        tokString = Lex.Ident "String" (makePosition 1 11 6)

        tok1      = Lex.NumberConst "1" (makePosition 1 1 1)
        tokMinus  = Lex.Symbol Lex.Minus (makePosition 1 1 1)

        tokA      = Lex.Ident "a" (makePosition 1 1 1)
        tokB      = Lex.Ident "b" (makePosition 1 5 1)
        tokPlus   = Lex.Symbol Lex.Plus (makePosition 1 3 1)

        tokErr1, tokErr2 :: Token
        tokErr1 = Lex.Ident "bad" (makePosition 2 1 3)
        tokErr2 = Lex.Symbol Lex.Assign (makePosition 2 5 1)

        tokCast, tokCastTy :: Token
        tokCast   = Lex.Symbol Lex.LParen (makePosition 3 1 1)
        tokCastTy = Lex.Ident "int" (makePosition 3 2 3)

        tokF, tokId :: Token
        tokF  = Lex.Ident "f"  (makePosition 4 1 1)
        tokId = Lex.Ident "id" (makePosition 5 1 2)

        tokTA1, tokTA2 :: Token
        tokTA1 = Lex.Ident "int"  (makePosition 5 4 3)
        tokTA2 = Lex.Ident "bool" (makePosition 5 9 4)


stmtTokensTests :: TestTree
stmtTokensTests = testGroup "Parse.SyntaxTree.stmtTokens" $ map (\(n, s, ts) -> testCase n $ stmtTokens s @=? ts) [
    ("0", Command Continue tokCmd, [tokCmd]),
    ("1", If (Variable "x" tokX)
        (Just (Multiple [Expr (Variable "a" tokA)]))
        (Just (Multiple [Expr (Variable "b" tokB)]))
        (tokIf, Just tokElse),
        [tokIf, tokX, tokA, tokElse, tokB]),
    ("2", Switch (Variable "x" tokX) [
        Case (IntConst "1" tok1) (Just $ Multiple [Expr (Variable "y" tokY)]) tokCase,
        Default (Multiple [Expr (Variable "z" tokZ)]) tokDefault
        ] tokSwitch,
        [tokSwitch, tokX, tok1, tokY, tokZ]),
    ("3", FunctionT (Int32T, [tokRet]) (Variable "f" tokF)
        [(Int32T, [tokTA1]), (Bool, [tokTA2])]
        [(Int32T, "x", [tokPx]), (Bool, "y", [tokPy])]
        (Multiple [Expr (Variable "body" tokBody)]),
        [tokRet, tokF, tokTA1, tokTA2, tokPx, tokPy, tokBody])]
    where
        tokCmd, tokIf, tokElse, tokX, tokA, tokB :: Token
        tokCmd  = Lex.Ident "cmd"  (makePosition 1 1 1)
        tokIf   = Lex.Ident "if"   (makePosition 1 3 2)
        tokElse = Lex.Ident "else" (makePosition 1 6 4)
        tokX    = Lex.Ident "x"    (makePosition 1 11 1)
        tokA    = Lex.Ident "a"    (makePosition 2 1 1)
        tokB    = Lex.Ident "b"    (makePosition 3 1 1)

        tokSwitch, tok1, tokY, tokZ, tokCase, tokDefault :: Token
        tokSwitch  = Lex.Ident "switch" (makePosition 4 1 6)
        tok1       = Lex.NumberConst "1" (makePosition 4 8 1)
        tokY       = Lex.Ident "y" (makePosition 4 10 1)
        tokZ       = Lex.Ident "z" (makePosition 4 12 1)
        tokCase    = Lex.Ident "case" (makePosition 4 14 4)
        tokDefault = Lex.Ident "default" (makePosition 4 19 7)

        tokRet, tokF, tokTA1, tokTA2, tokPx, tokPy, tokBody :: Token
        tokRet  = Lex.Ident "int"  (makePosition 5 1 3)
        tokF    = Lex.Ident "f"    (makePosition 5 5 1)
        tokTA1  = Lex.Ident "int"  (makePosition 5 7 3)
        tokTA2  = Lex.Ident "bool" (makePosition 5 11 4)
        tokPx   = Lex.Ident "x"    (makePosition 5 16 1)
        tokPy   = Lex.Ident "y"    (makePosition 5 18 1)
        tokBody = Lex.Ident "body" (makePosition 6 1 4)



promoteTopLevelFunctionsTests :: TestTree
promoteTopLevelFunctionsTests = testGroup "Parse.SyntaxTree.promoteTopLevelFunctions" [
    testCase "hoist nested functions with $ names and rewrite calls" $ do
        let tokFn = mkIdD "fn"
            tokRet = mkIdD "int"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokInner1 = mkIdD "inner1"
            tokInner2 = mkIdD "inner2"
            tokReturn = mkIdD "return"
            tokPlus = mkSymD Lex.Plus
            tokMul = mkSymD Lex.Multiply

            pA = (Int32T, "a", [tokA])
            pB = (Int32T, "b", [tokB])

            inner2Body = Multiple [
                Command (Return (Just (Binary Add (Variable "b" tokB) (IntConst "1" (mkNumD "1")) tokPlus))) tokReturn
                ]
            inner2Fun = Function (Int32T, [tokRet]) (Variable "inner2" tokInner2) [pB] inner2Body

            inner1Body = Multiple [
                inner2Fun,
                Command (Return (Just (
                    Binary Add
                        (Binary Mul (Variable "a" tokA) (IntConst "2" (mkNumD "2")) tokMul)
                        (Call (Variable "inner2" tokInner2) [Variable "b" tokB])
                        tokPlus
                    ))) tokReturn
                ]
            inner1Fun = Function (Int32T, [tokRet]) (Variable "inner1" tokInner1) [pA] inner1Body

            fooBody = Multiple [
                inner1Fun,
                Command (Return (Just (
                    Binary Mul
                        (Call (Variable "inner1" tokInner1) [Variable "a" tokA])
                        (IntConst "2" (mkNumD "2"))
                        tokMul
                    ))) tokReturn
                ]
            fooFun = Function (Int32T, [tokRet]) (Variable "foo" tokFn) [pA, pB] fooBody

            (_, outStmts) = promoteTopLevelFunctions ([], [fooFun])
            outFns = [ (name, declToks, map (\(_, n, _) -> n) params, body)
                     | Function (_, declToks) (Variable name _) params body <- outStmts ]

        map (\(name, _, _, _) -> name) outFns @?=
            ["foo$inner1$inner2", "foo$inner1", "foo"]

        let isPrivate (Lex.Ident "private" _) = True
            isPrivate _ = False
            isGeneratedName n = '$' `elem` n
            generatedDecls = [declToks | (name, declToks, _, _) <- outFns, isGeneratedName name]
        assertBool "generated functions should carry private token" $
            all (\declToks -> case declToks of
                    (t:_) -> isPrivate t
                    _ -> False) generatedDecls

        let lookupFn n = case [x | x@(name, _, _, _) <- outFns, name == n] of
                [x] -> x
                _ -> error ("function not found: " ++ n)

            (_, _, inner1Params, inner1BodyOut) = lookupFn "foo$inner1"
            (_, _, _, inner2BodyOut) = lookupFn "foo$inner1$inner2"
            (_, _, _, fooBodyOut) = lookupFn "foo"

            callNamesInExpr :: Expression -> [String]
            callNamesInExpr e = [name | x <- flattenExpr (Just e), Call (Variable name _) _ <- [x]]

            callNamesInBody :: Block -> [String]
            callNamesInBody (Multiple ss) = concat [
                callNamesInExpr e | Expr e <- ss ] ++ concat [
                maybe [] callNamesInExpr me | Command (Return me) _ <- ss
                ]
            inner1Calls = callNamesInBody inner1BodyOut
            fooCalls = callNamesInBody fooBodyOut
            inner2Calls = callNamesInBody inner2BodyOut

        inner1Params @?= ["a", "b"]
        assertBool ("inner1 calls: " ++ show inner1Calls) $
            "foo$inner1$inner2" `elem` inner1Calls
        assertBool ("foo calls: " ++ show fooCalls) $
            "foo$inner1" `elem` fooCalls
        assertBool ("inner2 calls: " ++ show inner2Calls) $
            "inner2" `notElem` inner2Calls
    ]

normalizeProgramForLoweringTests :: TestTree
normalizeProgramForLoweringTests = testGroup "Parse.SyntaxTree.normalizeProgramForLowering" [
    testCase "template blueprint is removed while concrete instance is kept" $ do
        let tokF = mkIdD "add"
            tokT = mkIdD "T"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokRet = mkIdD "return"
            tokI = mkIdD "int"
            tmpl = FunctionT
                (Class ["T"] [], [tokT])
                (Variable "add" tokF)
                [(Class ["T"] [], [tokT])]
                [ (Class ["T"] [], "a", [tokA])
                , (Class ["T"] [], "b", [tokB])
                ]
                (Multiple [Command (Return (Just (Variable "a" tokA))) tokRet])
            callStmt = Expr (CallT (Variable "add" tokF) [(Int32T, [tokI])]
                [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")])

            (_, outStmts) = normalizeProgramForLowering ([], [tmpl, callStmt])

            hasTemplateStmt = any isFunctionT outStmts
            hasGeneratedConcrete = any isGeneratedConcreteFun outStmts
            hasGeneratedPrivateFinal = any isGeneratedPrivateFinal outStmts
            hasCallTExpr = any isCallTExpr (flattenProgram ([], outStmts))

        assertBool "template declaration should be removed from lowering program" (not hasTemplateStmt)
        assertBool "lowering program should contain generated concrete function" hasGeneratedConcrete
        assertBool "generated concrete function should be private final" hasGeneratedPrivateFinal
        assertBool "all template calls should be rewritten before lowering" (not hasCallTExpr),

    testCase "template call with mismatched type argument count becomes syntax error expr" $ do
        let tokF = mkIdD "add"
            tokT = mkIdD "T"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokRet = mkIdD "return"
            tokI = mkIdD "int"
            tokB2 = mkIdD "bool"
            tmpl = FunctionT
                (Class ["T"] [], [tokT])
                (Variable "add" tokF)
                [(Class ["T"] [], [tokT])]
                [ (Class ["T"] [], "a", [tokA])
                , (Class ["T"] [], "b", [tokB])
                ]
                (Multiple [Command (Return (Just (Variable "a" tokA))) tokRet])
            badCallStmt = Expr (CallT (Variable "add" tokF) [(Int32T, [tokI]), (Bool, [tokB2])]
                [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")])

            (_, outStmts) = normalizeProgramForLowering ([], [tmpl, badCallStmt])
            flatExprs = flattenProgram ([], outStmts)
            errMsgs = [why | Error _ why <- flatExprs]
            hasCallTExpr = any isCallTExpr flatExprs

        assertBool "template type argument mismatch should become an error expression" $
            "template type argument count mismatch: add expects 1 type argument(s), got 2" `elem` errMsgs
        assertBool "all template calls should be rewritten before lowering" (not hasCallTExpr),

    testCase "empty template argument list infers type args from local hints" $ do
        let tokF = mkIdD "add"
            tokT = mkIdD "T"
            tokU = mkIdD "U"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokRet = mkIdD "return"
            tokX = mkIdD "x"
            tokY = mkIdD "y"
            tmpl = FunctionT
                (Class ["T"] [], [tokT])
                (Variable "add" tokF)
                [ (Class ["T"] [], [tokT])
                , (Class ["U"] [], [tokU])
                ]
                [ (Class ["T"] [], "a", [tokA])
                , (Class ["U"] [], "b", [tokB])
                ]
                (Multiple [Command (Return (Just (Variable "a" tokA))) tokRet])
            defX = DefConstField ["x"] (Just Int32T) (Just (IntConst "1" (mkNumD "1"))) [tokX]
            defY = DefConstField ["y"] (Just Int64T) (Just (LongConst "2" (mkNumD "2"))) [tokY]
            callStmt = Expr (CallT (Variable "add" tokF) []
                [Variable "x" tokX, Variable "y" tokY])
            (_, outStmts) = normalizeProgramForLowering ([], [tmpl, defX, defY, callStmt])
            flatExprs = flattenProgram ([], outStmts)
            hasCallTExpr = any isCallTExpr flatExprs
            generatedParamTypes = [map (\(t, _, _) -> t) ps | Function _ (Variable "add" _) ps _ <- outStmts]

        assertBool "all template calls should be rewritten before lowering" (not hasCallTExpr)
        assertBool "inferred concrete add(int, long) should be generated" $
            [Int32T, Int64T] `elem` generatedParamTypes,

    testCase "plain call falls back to template inference when no concrete overload exists" $ do
        let tokF = mkIdD "add"
            tokT = mkIdD "T"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokRet = mkIdD "return"
            tmpl = FunctionT
                (Class ["T"] [], [tokT])
                (Variable "add" tokF)
                [(Class ["T"] [], [tokT])]
                [ (Class ["T"] [], "a", [tokA])
                , (Class ["T"] [], "b", [tokB])
                ]
                (Multiple [Command (Return (Just (Variable "a" tokA))) tokRet])
            callStmt = Expr (Call (Variable "add" tokF)
                [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")])
            (_, outStmts) = normalizeProgramForLowering ([], [tmpl, callStmt])
            generatedParamTypes = [map (\(t, _, _) -> t) ps | Function _ (Variable "add" _) ps _ <- outStmts]
            hasCallTExpr = any isCallTExpr (flattenProgram ([], outStmts))

        assertBool "all template calls should be rewritten before lowering" (not hasCallTExpr)
        assertBool "plain call should infer and instantiate add(int, int)" $
            [Int32T, Int32T] `elem` generatedParamTypes,

    testCase "infer return-only template parameter from body expression" $ do
        let tokF = mkIdD "add"
            tokT = mkIdD "T"
            tokU = mkIdD "U"
            tokR = mkIdD "R"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokRet = mkIdD "return"
            tmpl = FunctionT
                (Class ["R"] [], [tokR])
                (Variable "add" tokF)
                [ (Class ["T"] [], [tokT])
                , (Class ["U"] [], [tokU])
                , (Class ["R"] [], [tokR])
                ]
                [ (Class ["T"] [], "a", [tokA])
                , (Class ["U"] [], "b", [tokB])
                ]
                (Multiple [Command (Return (Just (Binary Add (Variable "a" tokA) (Variable "b" tokB) (mkSymD Lex.Plus)))) tokRet])
            callStmt = Expr (CallT (Variable "add" tokF) []
                [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")])
            (_, outStmts) = normalizeProgramForLowering ([], [tmpl, callStmt])
            generatedSigs =
                [ (retT, map (\(t, _, _) -> t) ps)
                | Function (retT, _) (Variable "add" _) ps _ <- outStmts
                ]
            hasCallTExpr = any isCallTExpr (flattenProgram ([], outStmts))

        assertBool "all template calls should be rewritten before lowering" (not hasCallTExpr)
        assertBool "add<int,int,int> should be generated from return-type inference" $
            (Int32T, [Int32T, Int32T]) `elem` generatedSigs,

    testCase "merge all return value types to widest numeric type" $ do
        let tokF = mkIdD "add"
            tokT = mkIdD "T"
            tokU = mkIdD "U"
            tokR = mkIdD "R"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokRet1 = mkIdD "return"
            tokRet2 = mkIdD "return"
            tokIf = mkIdD "if"
            tokElse = mkIdD "else"
            tmpl = FunctionT
                (Class ["R"] [], [tokR])
                (Variable "add" tokF)
                [ (Class ["T"] [], [tokT])
                , (Class ["U"] [], [tokU])
                , (Class ["R"] [], [tokR])
                ]
                [ (Class ["T"] [], "a", [tokA])
                , (Class ["U"] [], "b", [tokB])
                ]
                (Multiple
                    [ If
                        (BoolConst True tokIf)
                        (Just (Multiple [Command (Return (Just (Binary Add (Variable "a" tokA) (Variable "b" tokB) (mkSymD Lex.Plus)))) tokRet1]))
                        (Just (Multiple [Command (Return (Just (LongConst "1L" (mkNumD "1")))) tokRet2]))
                        (tokIf, Just tokElse)
                    ])
            callStmt = Expr (CallT (Variable "add" tokF) []
                [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")])
            (_, outStmts) = normalizeProgramForLowering ([], [tmpl, callStmt])
            generatedSigs =
                [ (retT, map (\(t, _, _) -> t) ps)
                | Function (retT, _) (Variable "add" _) ps _ <- outStmts
                ]

        assertBool "add<int,int,long> should be generated from merged return types" $
            (Int64T, [Int32T, Int32T]) `elem` generatedSigs,

    testCase "mixed return and return-value should fail template return inference" $ do
        let tokF = mkIdD "add"
            tokT = mkIdD "T"
            tokU = mkIdD "U"
            tokR = mkIdD "R"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokRet1 = mkIdD "return"
            tokRet2 = mkIdD "return"
            tokIf = mkIdD "if"
            tokElse = mkIdD "else"
            tmpl = FunctionT
                (Class ["R"] [], [tokR])
                (Variable "add" tokF)
                [ (Class ["T"] [], [tokT])
                , (Class ["U"] [], [tokU])
                , (Class ["R"] [], [tokR])
                ]
                [ (Class ["T"] [], "a", [tokA])
                , (Class ["U"] [], "b", [tokB])
                ]
                (Multiple
                    [ If
                        (BoolConst True tokIf)
                        (Just (Multiple [Command (Return Nothing) tokRet1]))
                        (Just (Multiple [Command (Return (Just (Binary Add (Variable "a" tokA) (Variable "b" tokB) (mkSymD Lex.Plus)))) tokRet2]))
                        (tokIf, Just tokElse)
                    ])
            callStmt = Expr (CallT (Variable "add" tokF) []
                [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")])
            (_, outStmts) = normalizeProgramForLowering ([], [tmpl, callStmt])
            errMsgs = [why | Error _ why <- flattenProgram ([], outStmts)]

        assertBool "mixed return styles should report inference failure" $
            any (\msg -> "template type argument inference failed:" `isPrefixOf` msg) errMsgs
    ]
    where
        isGeneratedConcreteFun :: Statement -> Bool
        isGeneratedConcreteFun stmt = case stmt of
            Function _ (Variable name _) params _ -> name == "add" && length params == 2
            StaticMethod _ _ _ (Variable name _) params _ -> name == "add" && length params == 2
            InstanceMethod _ _ _ (Variable name _) params _ -> name == "add" && length params == 2
            _ -> False

        isGeneratedPrivateFinal :: Statement -> Bool
        isGeneratedPrivateFinal stmt = case stmt of
            Function (_, toks) (Variable name _) params _ -> name == "add" && length params == 2 && hasPrivateFinal toks
            StaticMethod _ _ (_, toks) (Variable name _) params _ -> name == "add" && length params == 2 && hasPrivateFinal toks
            InstanceMethod _ _ (_, toks) (Variable name _) params _ -> name == "add" && length params == 2 && hasPrivateFinal toks
            _ -> False

        hasPrivateFinal :: [Token] -> Bool
        hasPrivateFinal toks = hasWord "private" toks && hasWord "final" toks

        hasWord :: String -> [Token] -> Bool
        hasWord w = any (\t -> case t of
            Lex.Ident s _ -> map toLower s == w
            _ -> False)

        isCallTExpr :: Expression -> Bool
        isCallTExpr expr = case expr of
            CallT {} -> True
            _ -> False


normalizeStructProgramTests :: TestTree
normalizeStructProgramTests = testGroup "Parse.SyntaxTree.normalizeStructProgram" [
    testCase "rewrite struct static/member access and bare method call sugar" $ do
        let tokStruct = mkIdD "struct"
            tokPeople = mkIdD "people"
            tokPublic = mkIdD "public"
            tokStatic = mkIdD "static"
            tokVal = mkIdD "val"
            tokAge = mkIdD "age"
            tokConst = mkIdD "CONST"
            tokInit = mkIdD "__init__"
            tokDo = mkIdD "dosomething"
            tokThis = mkIdD "this"
            tokAssign = mkSymD Lex.Assign
            tokPlus = mkSymD Lex.Plus
            tokRet = mkIdD "return"
            tokMain = mkIdD "main"
            tokA = mkIdD "a"
            tok1 = mkNumD "1"
            tok10 = mkNumD "10"

            structStmt =
                Struct [] [] (tokStruct, Variable "people" tokPeople)
                    [ DefField ["age"] (Just Int32T) Nothing [tokPublic, tokVal, tokAge]
                    , DefConstField ["CONST"] (Just Int32T) (Just (IntConst "10" tok10)) [tokPublic, tokStatic, tokVal, tokConst]
                    , InstanceMethod [] [] (Void, [tokInit]) (Variable "__init__" tokInit)
                        [(Int32T, "age", [tokAge])]
                        (Multiple
                            [ Expr (Binary Assign
                                (Qualified ["this", "age"] [tokThis, tokAge])
                                (Variable "age" tokAge)
                                tokAssign)
                            ])
                    , InstanceMethod [] [] (Int32T, [tokDo]) (Variable "dosomething" tokDo)
                        []
                        (Multiple
                            [ Command (Return (Just
                                (Binary Add
                                    (Qualified ["this", "age"] [tokThis, tokAge])
                                    (Variable "CONST" tokConst)
                                    tokPlus))) tokRet
                            ])
                    ]

            mainStmt =
                Function (Void, [tokMain]) (Variable "main" tokMain) []
                    (Multiple
                        [ DefConstField ["a"] (Just (Class ["people"] []))
                            (Just (Call (Variable "people" tokPeople) [IntConst "1" tok1]))
                            [tokA]
                        , Expr (Call (Variable "dosomething" tokDo) [Variable "a" tokA])
                        , Expr (Variable "CONST" tokConst)
                        , Expr (Qualified ["a", "CONST"] [tokA, tokConst])
                        ])

            (_, outStmts) = normalizeProgram ([], [structStmt, mainStmt])

            idxOf predFn xs = fst <$> find (\(_, x) -> predFn x) (zip [0 :: Int ..] xs)
            idxConst = idxOf (\s -> case s of
                DefConstVar ["people$CONST"] _ _ _ -> True
                _ -> False) outStmts
            idxMain = idxOf (\s -> case s of
                Function _ (Variable "main" _) _ _ -> True
                _ -> False) outStmts

            mainBody = case [b | Function _ (Variable "main" _) _ b <- outStmts] of
                (b:_) -> b
                _ -> Multiple []
            flatMainExprs = flattenBlock (Just mainBody)
            hasRewrittenBareCall =
                any (\e -> case e of
                    Call (Variable "people$dosomething" _) _ -> True
                    _ -> False) flatMainExprs
            hasRewrittenStaticField =
                any (\e -> case e of
                    Variable "people$CONST" _ -> True
                    _ -> False) flatMainExprs
            rewrittenStaticFieldCount =
                length [() | Variable "people$CONST" _ <- flatMainExprs]
            methodBodyExprs = concat
                ([ flattenReturnExprs b
                 | InstanceMethod _ _ _ (Variable "people$dosomething" _) _ b <- outStmts
                 ] ++
                 [ flattenReturnExprs b
                 | StaticMethod _ _ _ (Variable "people$dosomething" _) _ b <- outStmts
                 ])
            hasRewrittenBareStaticInMethod =
                any (\e -> case e of
                    Variable "people$CONST" _ -> True
                    _ -> False) methodBodyExprs

        assertBool "generated static field should appear before main" $
            case (idxConst, idxMain) of
                (Just a, Just b) -> a < b
                _ -> False
        assertBool "bare method call should rewrite to struct global method call" hasRewrittenBareCall
        assertBool "instance static-field access should rewrite to struct static field symbol" hasRewrittenStaticField
        assertBool "unqualified static-field access in non-struct scope should rewrite when unique" (rewrittenStaticFieldCount >= 2)
        assertBool "unqualified static-field access in method should rewrite to struct static symbol" hasRewrittenBareStaticInMethod
    ,
    testCase "rewrite bare static method call inside struct static method" $ do
        let tokStruct = mkIdD "struct"
            tokScreen = mkIdD "Screen"
            tokSetChar = mkIdD "setChar"
            tokClear = mkIdD "clear"
            tokRet = mkIdD "return"
            tokSpace = Lex.CharConst ' ' (makePosition 0 0 0)
            declStatic = [(Public, [Static])]

            setCharMethod =
                InstanceMethod declStatic [] (Void, [tokSetChar]) (Variable "setChar" tokSetChar)
                    [(Char, "c", [mkIdD "c"])]
                    (Multiple [])

            clearMethod =
                InstanceMethod declStatic [] (Void, [tokClear]) (Variable "clear" tokClear)
                    []
                    (Multiple
                        [ Expr (Call (Variable "setChar" tokSetChar) [CharConst ' ' tokSpace])
                        , Command (Return Nothing) tokRet
                        ])

            structStmt =
                Struct [] [] (tokStruct, Variable "Screen" tokScreen) [setCharMethod, clearMethod]

            (_, outStmts) = normalizeProgram ([], [structStmt])

            staticClearBodies =
                [ b
                | StaticMethod _ _ _ (Variable "Screen$clear" _) _ b <- outStmts
                ] ++
                [ b
                | InstanceMethod _ _ _ (Variable "Screen$clear" _) _ b <- outStmts
                ]

            hasRewrittenCall =
                any (\b -> any isScreenSetCharCall (flattenBlock (Just b))) staticClearBodies

        assertBool "bare static call should rewrite to Screen$setChar" hasRewrittenCall
    ,
    testCase "rewrite Struct.STATIC_INSTANCE.instanceMethod(...) call sugar" $ do
        let tokStruct = mkIdD "struct"
            tokDisplay = mkIdD "Display"
            tokStatic = mkIdD "static"
            tokVal = mkIdD "val"
            tokInstance = mkIdD "INSTANCE"
            tokPrint = mkIdD "print"
            tokMain = mkIdD "main"
            tokOne = mkNumD "1"

            instanceField =
                DefConstField ["INSTANCE"] (Just (Class ["Display"] []))
                    (Just (Call (Variable "Display" tokDisplay) []))
                    [tokStatic, tokVal, tokInstance]

            printMethod =
                InstanceMethod [] [] (Void, [tokPrint]) (Variable "print" tokPrint)
                    [(Int32T, "x", [mkIdD "x"])]
                    (Multiple [])

            structStmt =
                Struct [] [] (tokStruct, Variable "Display" tokDisplay)
                    [instanceField, printMethod]

            mainStmt =
                Function (Void, [tokMain]) (Variable "main" tokMain) []
                    (Multiple
                        [ Expr (Call (Qualified ["Display", "INSTANCE", "print"] [tokDisplay, tokInstance, tokPrint]) [IntConst "1" tokOne])
                        ])

            (_, outStmts) = normalizeProgram ([], [structStmt, mainStmt])
            mainBody = case [b | Function _ (Variable "main" _) _ b <- outStmts] of
                (b:_) -> b
                _ -> Multiple []
            flatMainExprs = flattenBlock (Just mainBody)
            hasRewrittenCall =
                any (\e -> case e of
                    Call (Variable "Display$print" _) (Variable "Display$INSTANCE" _ : _) -> True
                    CallT (Variable "Display$print" _) _ (Variable "Display$INSTANCE" _ : _) -> True
                    _ -> False) flatMainExprs

        assertBool "Display.INSTANCE.print(...) should rewrite to Display$print(Display$INSTANCE, ...)" hasRewrittenCall
    ,
    testCase "rewrite new Struct(...) into heap alloc + __init__ call" $ do
        let tokStruct = mkIdD "struct"
            tokDisplay = mkIdD "Display"
            tokInit = mkIdD "__init__"
            tokMain = mkIdD "main"
            tokA = mkIdD "a"
            tokNew = mkIdD "new"
            tok1 = mkNumD "1"

            structStmt =
                Struct [] [] (tokStruct, Variable "Display" tokDisplay)
                    [ InstanceMethod [] [] (Void, [tokInit]) (Variable "__init__" tokInit)
                        [(Int32T, "x", [mkIdD "x"])]
                        (Multiple [])
                    ]

            mainStmt =
                Function (Void, [tokMain]) (Variable "main" tokMain) []
                    (Multiple
                        [ DefConstField ["a"] (Just (Class ["Display"] []))
                            (Just (Call (Unary AddrOf (Variable "Display" tokDisplay) tokNew) [IntConst "1" tok1]))
                            [tokA]
                        ])

            (_, outStmts) = normalizeProgram ([], [structStmt, mainStmt])
            mainBody = case [b | Function _ (Variable "main" _) _ b <- outStmts] of
                (b:_) -> b
                _ -> Multiple []
            flatMainExprs = flattenBlock (Just mainBody)

            hasAllocCall =
                any (\e -> case e of
                    Call (Qualified ["xlang", "System", "allocMemory"] _) [IntConst _ _] -> True
                    _ -> False) flatMainExprs

            hasInitCall =
                any (\e -> case e of
                    Call (Variable "Display$__init__" _) (_ : _) -> True
                    CallT (Variable "Display$__init__" _) _ (_ : _) -> True
                    _ -> False) flatMainExprs

        assertBool "new Display(...) should call xlang.System.allocMemory(...)" hasAllocCall
        assertBool "new Display(...) should call Display$__init__(this,...)" hasInitCall
    ,
    testCase "struct layout keeps 8-byte alignment and blob size contribution" $ do
        let tokStruct = mkIdD "struct"
            tokTest = mkIdD "test"
            tokVal = mkIdD "val"
            tokA = mkIdD "a"
            tokB = mkIdD "b"
            tokC = mkIdD "c"
            tokD = mkIdD "d"
            tok998 = mkNumD "998"
            st =
                Struct [] [] (tokStruct, Variable "test" tokTest)
                    [ DefField ["a"] (Just Int32T) Nothing [tokVal, tokA]
                    , DefField ["b"] (Just (Blob (IntConst "998" tok998))) Nothing [tokVal, tokB]
                    , DefField ["c"] (Just (Pointer Char)) Nothing [tokVal, tokC]
                    , DefField ["d"] (Just Float64T) Nothing [tokVal, tokD]
                    ]
            meta = buildStructMeta st
            off name = sfOffset <$> Map.lookup name (smFields meta)

        smName meta @=? "test"
        off "a" @=? Just 0
        off "b" @=? Just 8
        off "c" @=? Just 1008
        off "d" @=? Just 1016
        smSize meta @=? 1024
    ]
  where
    flattenReturnExprs :: Block -> [Expression]
    flattenReturnExprs (Multiple ss) = concatMap fromStmt ss

    fromStmt :: Statement -> [Expression]
    fromStmt st = case st of
        Command (Return (Just e)) _ -> flattenExpr (Just e)
        BlockStmt b -> flattenReturnExprs b
        StmtGroup ss -> concatMap fromStmt ss
        If _ b1 b2 _ -> maybe [] flattenReturnExprs b1 ++ maybe [] flattenReturnExprs b2
        For (_, _, _) b1 b2 _ -> maybe [] flattenReturnExprs b1 ++ maybe [] flattenReturnExprs b2
        While _ b1 b2 _ -> maybe [] flattenReturnExprs b1 ++ maybe [] flattenReturnExprs b2
        Until _ b1 b2 _ -> maybe [] flattenReturnExprs b1 ++ maybe [] flattenReturnExprs b2
        DoWhile b1 _ b2 _ -> maybe [] flattenReturnExprs b1 ++ maybe [] flattenReturnExprs b2
        DoUntil b1 _ b2 _ -> maybe [] flattenReturnExprs b1 ++ maybe [] flattenReturnExprs b2
        Repeat _ b1 b2 _ -> maybe [] flattenReturnExprs b1 ++ maybe [] flattenReturnExprs b2
        Switch _ scs _ -> concatMap fromCase scs
        _ -> []

    fromCase :: SwitchCase -> [Expression]
    fromCase sc = case sc of
        Case _ mb _ -> maybe [] flattenReturnExprs mb
        Default b _ -> flattenReturnExprs b

    isScreenSetCharCall :: Expression -> Bool
    isScreenSetCharCall expr = case expr of
        Call (Variable "Screen$setChar" _) _ -> True
        CallT (Variable "Screen$setChar" _) _ _ -> True
        _ -> False


prettyExprTests :: TestTree
prettyExprTests = testGroup "Parse.SyntaxTree.prettyExpr" $ map (\(i, n, inp, out) -> testCase i $ prettyExpr n inp @=? out) [
    ("0", 0, Nothing, ""),
    ("1", 1, Just (IntConst "1" (mkNumD "1")), insertSpace 4 ++ "1"),
    ("2", 2, Just (BoolConst True (mkIdD "true")), insertSpace 8 ++ "true"),
    ("3", 1, Just (BoolConst False (mkIdD "false")), insertSpace 4 ++ "false"),
    ("4", 0, Just (Variable "x" (mkIdD "x")), "x"),
    ("5", 0, Just (Qualified ["a","b","c"] [mkIdD "a"]), "a.b.c"),
    ("6", 0, Just (Cast (Class ["Int"] [], [mkIdD "Int"]) (IntConst "1" (mkNumD "1")) (mkIdD "cast")), "(_Int)(1)"),
    ("7", 0, Just (Binary Add
        (Unary Sub (Variable "x" (mkIdD "x")) (mkIdD "-"))
        (IntConst "2" (mkNumD "2"))
        (mkIdD "+")), "-x+2")]


prettyBlockTests :: TestTree
prettyBlockTests = testGroup "Parse.SyntaxTree.prettyBlock" $ map (\(i, n, inp, out) ->
    testCase i $ prettyBlock n inp @=? out) [
    ("0", 0, Multiple [], "{\n\n}\n"),
    ("1", 0, Multiple [Expr (Variable "a" dummyTok)], "{\n    a\n\n}\n"),
    ("2", 0, Multiple [Expr (Variable "a" dummyTok), Expr (IntConst "1" dummyTok)], "{\n    a\n    1\n\n}\n"),
    ("3", 0, Multiple [BlockStmt
        (Multiple
            [Expr (Variable "x" dummyTok)])], "{\n    {\n        x\n\n    }\n\n}\n")]
    where
        dummyTok :: Token
        dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


prettyStmtTests :: TestTree
prettyStmtTests = testGroup "Parse.SyntaxTree.prettyStmt" $ map (\(i, n, inp, out) ->
    testCase i $ prettyStmt n inp @=? out) [
    ("0", 0, Nothing, "\n"),
    ("1", 0, Just (Expr (Variable "a" dummyTok)), "a\n"),
    ("2", 0, Just (BlockStmt (Multiple [])), "{\n\n}\n"),
    ("3", 0, Just (If (Variable "a" dummyTok) Nothing Nothing (dummyTok, Nothing)), "if (a);\n"),
    ("4", 0, Just
        (While
            (Variable "a" dummyTok)
            (Just (Multiple []))
            Nothing
            (dummyTok, Nothing)),
        "while(a)\n{\n\n}\n\n")]
  where
    dummyTok :: Token
    dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


prettyProgmTests :: TestTree
prettyProgmTests = testGroup "Parse.SyntaxTree.prettyProgm" $ map (\(i, inp, out) ->
    testCase i $ do
            got <- prettyProgram inp
            got @=? out) [
    ("0" , ([], []), ""),
    ("1", ([Package ["java", "lang", "math"] []], [
        Expr (Variable "a" dummyTok),
        Expr (Binary Add (Variable "a" dummyTok) (IntConst "1" dummyTok) dummyTok)]),
        unlines [ "package java.lang.math", "a", "a+1"]),
        
    ("2", ([], [
        BlockStmt (Multiple [
            Expr (Variable "x" dummyTok),
            Expr (Binary Mul (Variable "x" dummyTok) (IntConst "2" dummyTok) dummyTok)])]),
            
        unlines[
            "{",
            "    x",
            "    x*2",
            "}"]),
            
    ("3", ([], [
        If (Binary GreaterThan (Variable "a" dummyTok) (IntConst "0" dummyTok) dummyTok)
            (Just (Multiple [
                Expr (Variable "pos" dummyTok),
                Command (Return (Just (IntConst "1" dummyTok))) dummyTok]))
            (Just (Multiple [
                Expr (Variable "neg" dummyTok),
                Command (Return (Just (IntConst "0" dummyTok))) dummyTok]))
            (dummyTok, Just dummyTok)]),
                
        unlines [
            "if (a>0)",
            "{",
            "    pos",
            "    return 1",
            "}",
            "else",
            "{",
            "    neg",
            "    return 0",
            "}"]),
            
    ("4", ([], [
        While (Binary NotEqual (Variable "x" dummyTok) (IntConst "0" dummyTok) dummyTok)
            (Just (Multiple
                [Expr (Binary Sub (Variable "x" dummyTok) (IntConst "1" dummyTok) dummyTok),
            
            BlockStmt (Multiple [
                    Expr (Variable "inner" dummyTok),
                    Command Continue dummyTok])]))
            Nothing
            (dummyTok, Nothing)]),
        unlines [
            "while(x!=0)",
            "{",
            "    x-1",
            "    {",
            "        inner",
            "        continue",
            "    }",
            "}"]),
            
    ("5", ([], [
        DoWhile (Just (Multiple [
            Expr (Variable "tick" dummyTok),
            Command Break dummyTok]))
            (Binary LessThan (Variable "t" dummyTok) (IntConst "100" dummyTok) dummyTok) Nothing (dummyTok, dummyTok, Nothing)]),
        
        unlines [
            "do",
            "{",
            "    tick",
            "    break",
            "}",
            "while(t<100)"]),
            
    ("6", ([], [
        For (Just (Expr (Binary Assign (Variable "i" dummyTok) (IntConst "0" dummyTok) dummyTok)),
             Just (Binary LessThan (Variable "i" dummyTok) (IntConst "10" dummyTok) dummyTok),
             Just (Expr (Unary SelfInc (Variable "i" dummyTok) dummyTok)))
                (Just (Multiple [
                    Expr (Binary Add (Variable "sum" dummyTok) (Variable "i" dummyTok) dummyTok)]))
                Nothing
                (dummyTok, Nothing)]),
        
        unlines [
            "for(i=0;i<10;++i)",
            "{",
            "    sum+i",
            "}"]),
            

    ("7", ([Package ["com", "wangdi"] []], [
        Expr (Binary Assign (Variable "a" dummyTok) (IntConst "1" dummyTok) dummyTok),
            If (Binary Equal (Variable "a" dummyTok) (IntConst "1" dummyTok) dummyTok)
                (Just (Multiple [
                    Expr (Binary Assign (Variable "b" dummyTok) (IntConst "2" dummyTok) dummyTok),
                    Expr (Binary Add (Variable "a" dummyTok) (Variable "b" dummyTok) dummyTok)]))
                Nothing
                (dummyTok, Nothing),
                Command (Return (Just (Variable "a" dummyTok))) dummyTok]),
        init $ unlines [
            "package com.wangdi",
            "a=1",
            "if (a==1)",
            "{",
            "    b=2",
            "    a+b",
            "}",
            "return a"])]
    where
        dummyTok :: Token
        dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


prettyDeclarationTests :: TestTree
prettyDeclarationTests = testGroup "Parse.SyntaxTree.prettyDeclaration" $
    map (\(i, inp, out) -> testCase i $ prettyDeclaration inp @=? out) [
        ("0", Package [] [], "package "),
        ("1", Package ["a"] [], "package a"),
        ("2", Import [] [], "Import "),
        ("3", Import ["a","b","c"] [], "Import a.b.c"),
        ("4", JavaName "MyUtils" (mkIdD "MyUtils"), "classname \"MyUtils\"")]



tests :: TestTree
tests = testGroup "Parse.SyntaxTree" [
    flattenExprTests, flattenBlockTests, flattenCaseTests, flattenStatementTests, flattenProgramTests,

    getErrorProgramTests, normalizeClassTests, classMangleDemangleTests,
    isVariableTests, isAtomTests, identTextTests, numTextTests, charValTests, strValTests, exprTokensTests, stmtTokensTests,
    
    prettyExprTests, prettyBlockTests, prettyStmtTests, prettyProgmTests, prettyDeclarationTests,
     
    isPackageDeclTests, isImportDeclTests, isClassDeclarTests,
    isFunctionTests, isFunctionTTests, isAssignmentTests, declPathTests, getPackageTests,
    collectInputProgramTests, collectInputProgramsTests, promoteTopLevelFunctionsTests, normalizeProgramForLoweringTests, normalizeStructProgramTests]
    
