module Parse.SyntaxTreeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Util.Type
import Util.Basic
import Lex.Token (Token, Symbol)

import qualified Lex.Token as Lex


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

    ("4", Just (For (Just (IntConst "1" $ mkNumD "1"), Just (IntConst "2" $ mkNumD "2"), Just (IntConst "3" $ mkNumD "3")) (Just (Multiple [Expr (IntConst "4" $ mkNumD "4")])) dummyTok),
        [IntConst "1" $ mkNumD "1", IntConst "2" $ mkNumD "2", IntConst "3" $ mkNumD "3", IntConst "4" $ mkNumD "4"]),

    ("5", Just (For (Nothing, Just (IntConst "2" $ mkNumD "2"), Nothing) Nothing dummyTok), [IntConst "2" $ mkNumD "2"]),

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
        ("2", Expr (Binary Add (Variable "a" $ mkIdD "a") (IntConst "1" $ mkNumD "1") (mkSymD Lex.Plus)), False),
        ("3", Command (Return Nothing) (mkIdD "return"), False)
    ]


declPathTests :: TestTree
declPathTests = testGroup "Parse.SyntaxTree.declPath" $
    map (\(i, decl, out) -> testCase i $ declPath decl @=? out) [
        ("0", Package ["a"] [], ["a"]),
        ("1", Package ["java", "lang", "util"] [], ["java", "lang", "util"]),
        ("2", Import ["math"] [], ["math"]),
        ("3", Import ["java", "util", "List"] [], ["java", "util", "List"])]


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
        ("1", Array (Class ["int"] []) 2, Array Int32T 2),
        ("2", Class ["List"] [Class ["int"] []], Class ["List"] [Int32T]),
        ("3", Class ["int"] [Class ["int"] []], Class ["int"] [Int32T])]


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
        For (Just (Binary Assign (Variable "i" dummyTok) (IntConst "0" dummyTok) dummyTok),
             Just (Binary LessThan (Variable "i" dummyTok) (IntConst "10" dummyTok) dummyTok),
             Just (Unary SelfInc (Variable "i" dummyTok) dummyTok))
                (Just (Multiple [
                    Expr (Binary Add (Variable "sum" dummyTok) (Variable "i" dummyTok) dummyTok)]))
                dummyTok]),
        
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
        ("3", Import ["a","b","c"] [], "Import a.b.c")]



tests :: TestTree
tests = testGroup "Parse.SyntaxTree" [
    flattenExprTests, flattenBlockTests, flattenCaseTests, flattenStatementTests, flattenProgramTests,

    getErrorProgramTests, normalizeClassTests,
    isVariableTests, isAtomTests, identTextTests, numTextTests, charValTests, strValTests, exprTokensTests, stmtTokensTests,
    
    prettyExprTests, prettyBlockTests, prettyStmtTests, prettyProgmTests, prettyDeclarationTests,
    
    isPackageDeclTests, isImportDeclTests, isClassDeclarTests,
    isFunctionTests, isFunctionTTests, isAssignmentTests, declPathTests]
