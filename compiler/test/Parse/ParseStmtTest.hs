module Parse.ParseStmtTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseStmt
import Parse.ParseProgm
import Util.Type
import Lex.Token (Token, Symbol)
import Util.Exception

import qualified Lex.Token as Lex


mkSym :: Symbol -> Int -> Int -> Int -> Token
mkSym s a b c = Lex.Symbol s $ Position a b c

mkNum :: String -> Int -> Int -> Int -> Token
mkNum s a b c = Lex.NumberConst s $ Position a b c

mkId :: String -> Int -> Int -> Int -> Token
mkId s a b c = Lex.Ident s $ makePosition a b c


normalTests :: TestTree
normalTests = testGroup "Parse.ParseStmt.lexparseStmt" $ map (\(n, src, expected) ->
    testCase n $ replLexparseStmt src @=? expected) [
        ("0", "1 + 2 * 3;", Right $ Expr (
            Binary Add
                (IntConst "1" $ mkNum "1" 1 1 1)
                (Binary Mul
                    (IntConst "2" $ mkNum "2" 1 5 1)
                    (IntConst "3" $ mkNum "3" 1 9 1)
                    (mkSym Lex.Multiply 1 7 1))
                (mkSym Lex.Plus 1 3 1))),

        ("0a", "x = 5 % 2;", Right $ Expr (
            Binary Assign
                (Variable "x" (mkId "x" 1 1 1))
                (Binary Mod
                    (IntConst "5" (mkNum "5" 1 5 1))
                    (IntConst "2" (mkNum "2" 1 9 1))
                    (mkSym Lex.Modulo 1 7 1))
                (mkSym Lex.Assign 1 3 1))),

        ("1", "{ 1 + 2; 3 * 4; }", Right $ BlockStmt (
            Multiple
            [Expr (Binary Add
                (IntConst "1" $ mkNum "1" 1 3 1)
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 5 1))
            , Expr (Binary Mul
                (IntConst "3" $ mkNum "3" 1 10 1)
                (IntConst "4" $ mkNum "4" 1 14 1)
                (mkSym Lex.Multiply 1 12 1))])),
                
        ("2", "{ { 1; } 2; }", Right $ BlockStmt (
            Multiple
            [BlockStmt (Multiple
                [Expr (IntConst "1" $ mkNum "1" 1 5 1)]),
                Expr (IntConst "2" $ mkNum "2" 1 10 1)])),
                
        ("3", "{ 1 + 2\n  3 + 4\n}", Right $ BlockStmt (
            Multiple
            [Expr (Binary Add
                (IntConst "1" $ mkNum "1" 1 3 1)
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 5 1)),
            
            Expr (Binary Add
                (IntConst "3" $ mkNum "3" 2 3 1)
                (IntConst "4" $ mkNum "4" 2 7 1)
                (mkSym Lex.Plus 2 5 1))])),

        ("4", unlines [
            "while true {",
            "    x = x + 1",
            "}"],
        
            Right $ While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    Expr (
                        Binary Assign
                            (Variable "x" (mkId "x" 2 5 1))
                            (Binary Add
                                (Variable "x" (mkId "x" 2 9 1))
                                (IntConst "1" (mkNum "1" 2 13 1))
                                (mkSym Lex.Plus 2 11 1))
                            (mkSym Lex.Assign 2 7 1))
                ]))
                Nothing
                (mkId "while" 1 1 5, Nothing)),

        ("5",
            unlines [
                "while true:",
                "    while false:",
                "        aa = aa + 2"
            ],
            Right $ While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    While
                        (BoolConst False (mkId "false" 2 11 5))
                        (Just (Multiple [
                            Expr (
                                Binary Assign
                                    (Variable "aa" (mkId "aa" 3 9 2))
                                    (Binary Add
                                        (Variable "aa" (mkId "aa" 3 14 2))
                                        (IntConst "2" (mkNum "2" 3 19 1))
                                        (mkSym Lex.Plus 3 17 1))
                                    (mkSym Lex.Assign 3 12 1))]))
                        Nothing
                        (mkId "while" 2 5 5, Nothing)
                ]))
                Nothing
                (mkId "while" 1 1 5, Nothing)
        ),

        -- while true
        --   x = x + 1
        --   x = x + 2
        ("6",
            unlines [
                "while true:",
                "    x = x + 1",
                "    x = x + 2"
            ],
            Left [Parsing $ makeError 
                "stdin" [makePosition 3 5 1] "invalid syntax: Ident \"x\" (3,5,1)"
            ]),


        -- while x + 2 { a }
        ("7",
            unlines [
                "while x + 2:",
                "{",
                "    a",
                "}"
            ],
            Right $ While
                (Binary Add
                    (Variable "x" (mkId "x" 1 7 1))
                    (IntConst "2" (mkNum "2" 1 11 1))
                    (mkSym Lex.Plus 1 9 1))
                (Just (Multiple [BlockStmt  (Multiple [
                    Expr (Variable "a" (mkId "a" 3 5 1))
                ])]))
                Nothing
                (mkId "while" 1 1 5, Nothing))]


valStmtTests :: TestTree
valStmtTests = testCase "val_0" $
    case replLexparseStmt "val pi = 3;" of
        Right (DefConstField ["pi"] _ _ _) -> pure ()
        other -> assertFailure ("expected DefConstField, got: " ++ show other)

typedDeclSyntaxTests :: TestTree
typedDeclSyntaxTests = testGroup "typed_decl_syntax"
    [ testCase "var_multi_mixed_types" $
        case replLexparseStmt "var a: int = 1, b: double = 2;" of
            Right (StmtGroup
                [ DefField ["a"] (Just Int32T) (Just (IntConst "1" _)) _
                , DefField ["b"] (Just Float64T) (Just (IntConst "2" _)) _
                ]) -> pure ()
            other -> assertFailure ("expected multi var decl block, got: " ++ show other)
    , testCase "var_multi_infer" $
        case replLexparseStmt "var a = 10, b = 20;" of
            Right (StmtGroup
                [ DefField ["a"] Nothing (Just (IntConst "10" _)) _
                , DefField ["b"] Nothing (Just (IntConst "20" _)) _
                ]) -> pure ()
            other -> assertFailure ("expected multi inferred var decl block, got: " ++ show other)
    ]

loopSyntaxTests :: TestTree
loopSyntaxTests = testGroup "loop_syntax"
    [ testCase "loop_removed" $
        case replLexparseStmt "loop;" of
            Left _ -> pure ()
            other -> assertFailure ("expected parse error after removing loop syntax, got: " ++ show other)

    , testCase "until_else_block" $
        case replLexparseStmt (unlines
            [ "until false:"
            , "    x = 1"
            , "else:"
            , "    x = 2"
            ]) of
            Right (Until
                (BoolConst False _)
                (Just (Multiple [Expr (Binary Assign (Variable "x" _) (IntConst "1" _) _)]))
                (Just (Multiple [Expr (Binary Assign (Variable "x" _) (IntConst "2" _) _)]))
                _) -> pure ()
            other -> assertFailure ("expected until-else statement, got: " ++ show other)

    , testCase "for_var_multi_init_and_expr_step_list" $
        case replLexparseStmt "for (var i: int = 0, sum: int = 0; i < 10; i++, sum = sum + i);" of
            Right (For
                ( Just (StmtGroup
                    [ DefField ["i"] (Just Int32T) (Just (IntConst "0" _)) _
                    , DefField ["sum"] (Just Int32T) (Just (IntConst "0" _)) _
                    ])
                , Just (Binary LessThan (Variable "i" _) (IntConst "10" _) _)
                , Just (Exprs
                    [ Unary SelfInc (Variable "i" _) _
                    , Binary Assign (Variable "sum" _)
                        (Binary Add (Variable "sum" _) (Variable "i" _) _)
                        _
                    ])
                )
                Nothing
                Nothing
                _) -> pure ()
            other -> assertFailure ("expected for with multi init/step list, got: " ++ show other)
    ]

ifElifSyntaxTests :: TestTree
ifElifSyntaxTests = testGroup "if_elif_syntax"
    [ testCase "if_elif_else_desugars_to_nested_if" $
        case replLexparseStmt (unlines
            [ "if a:"
            , "    x = 1"
            , "elif b:"
            , "    x = 2"
            , "else:"
            , "    x = 3"
            ]) of
            Right (If
                (Variable "a" _)
                (Just (Multiple [Expr (Binary Assign (Variable "x" _) (IntConst "1" _) _)]))
                (Just (Multiple
                    [ If
                        (Variable "b" _)
                        (Just (Multiple [Expr (Binary Assign (Variable "x" _) (IntConst "2" _) _)]))
                        (Just (Multiple [Expr (Binary Assign (Variable "x" _) (IntConst "3" _) _)]))
                        _
                    ]))
                _) -> pure ()
            other -> assertFailure ("expected nested If desugaring for elif, got: " ++ show other)
    ]

ifColonNewlineStmtTests :: TestTree
ifColonNewlineStmtTests = testGroup "if_colon_newline_stmt"
    [ testCase "if body after ':' newline parses as statement" $
        case replLexparseStmt (unlines
            [ "if a:"
            , "    b()"
            ]) of
            Right (If
                (Variable "a" _)
                (Just (Multiple [Expr (Call (Variable "b" _) [])]))
                Nothing
                _) -> pure ()
            other -> assertFailure ("expected If statement with newline body, got: " ++ show other)

    , testCase "if cast-bool condition with ':' newline body parses as statement" $
        case replLexparseStmt (unlines
            [ "if (x and 1) as bool:"
            , "    mulByFibQ(mat)"
            ]) of
            Right (If
                (Cast (Bool, _) (Binary BitAnd (Variable "x" _) (IntConst "1" _) _) _)
                (Just (Multiple [Expr (Call (Variable "mulByFibQ" _) [Variable "mat" _])]))
                Nothing
                _) -> pure ()
            other -> assertFailure ("expected If statement for cast-bool condition, got: " ++ show other)

    , testCase "if body supports bare return without braces" $
        case replLexparseStmt (unlines
            [ "if size <= 1:"
            , "    return"
            ]) of
            Right (If
                (Binary LessEqual (Variable "size" _) (IntConst "1" _) _)
                (Just (Multiple [Command (Return Nothing) _]))
                Nothing
                _) -> pure ()
            other -> assertFailure ("expected If statement with bare return body, got: " ++ show other)

    , testCase "if body supports value return without braces" $
        case replLexparseStmt (unlines
            [ "if size <= 0:"
            , "    return 0 as A"
            ]) of
            Right (If
                (Binary LessEqual (Variable "size" _) (IntConst "0" _) _)
                (Just (Multiple [Command (Return (Just (Cast (Class ["A"] _, _) (IntConst "0" _) _))) _]))
                Nothing
                _) -> pure ()
            other -> assertFailure ("expected If statement with value return body, got: " ++ show other)

    , testCase "if/elif with block return parses without colon-return ambiguity" $
        case replLexparseStmt (unlines
            [ "if x > 0: { return; }"
            , "elif x == 0: { return; }"
            , "else: { return; }"
            ]) of
            Right (If
                (Binary GreaterThan (Variable "x" _) (IntConst "0" _) _)
                (Just (Multiple [Command (Return Nothing) _]))
                (Just (Multiple [If
                    (Binary Equal (Variable "x" _) (IntConst "0" _) _)
                    (Just (Multiple [Command (Return Nothing) _]))
                    (Just (Multiple [Command (Return Nothing) _]))
                    _]))
                _) -> pure ()
            other -> assertFailure ("expected if/elif/else with block return, got: " ++ show other)
    ]

returnIfExprTests :: TestTree
returnIfExprTests = testGroup "return_if_expr"
    [ testCase "return accepts if-expression with elif/else" $
        case replLexparseStmt "return if a <= 1: a elif a == 2: 1 else: (fib(a - 1) + fib(a - 2));" of
            Right (Command (Return (Just (IfExpr
                (Binary LessEqual (Variable "a" _) (IntConst "1" _) _)
                (Variable "a" _)
                (IfExpr
                    (Binary Equal (Variable "a" _) (IntConst "2" _) _)
                    (IntConst "1" _)
                    (Binary Add
                        (Call (Variable "fib" _) [Binary Sub (Variable "a" _) (IntConst "1" _) _])
                        (Call (Variable "fib" _) [Binary Sub (Variable "a" _) (IntConst "2" _) _])
                        _)
                    _)
                _))) _) -> pure ()
            other -> assertFailure ("expected return IfExpr statement, got: " ++ show other)
    ]

pointerDerefAssignStmtTests :: TestTree
pointerDerefAssignStmtTests = testGroup "pointer_deref_assign_stmt"
    [ testCase "parenthesized deref assignment parses as statement" $
        case replLexparseStmt "(arr + i).deref = i;" of
            Right (Expr (Binary Assign (Unary DeRef _ _) (Variable "i" _) _)) -> pure ()
            other -> assertFailure ("expected deref assignment statement, got: " ++ show other)

    , testCase "newline before parenthesized deref assignment is statement separator" $
        case replLexparseStmt (unlines
            [ "{"
            , "val arr = 0 as pointer<int>"
            , "val i = 0"
            , "(arr + i).deref = i"
            , "}"
            ]) of
            Right (BlockStmt (Multiple
                [ DefConstField ["arr"] _ _ _
                , DefConstField ["i"] _ _ _
                , Expr (Binary Assign (Unary DeRef _ _) (Variable "i" _) _)
                ])) -> pure ()
            other -> assertFailure ("expected block with deref assignment line, got: " ++ show other)
    ]

inlineFunctionSyntaxTests :: TestTree
inlineFunctionSyntaxTests = testGroup "inline_function_syntax"
    [ testCase "0" $
        case replLexparseStmt "inline fun add(a: int, b: int) -> int = a + b;" of
            Right (Function (Int32T, retToks) (Variable "add" _) _ (Multiple [Command (Return (Just _)) _])) ->
                assertBool "inline token should exist in return-token list" (hasInlineTok retToks)
            other -> assertFailure ("expected inline function statement, got: " ++ show other)
    , testCase "1" $
        case replLexparseStmt "inline var x: int = 1;" of
            Left _ -> pure ()
            Right stmt -> assertFailure ("expected parse failure for inline var, got: " ++ show stmt)
    ]
    where
        hasInlineTok :: [Token] -> Bool
        hasInlineTok = any isInlineTok

        isInlineTok :: Token -> Bool
        isInlineTok (Lex.Ident "inline" _) = True
        isInlineTok _ = False

mutParamSyntaxTests :: TestTree
mutParamSyntaxTests = testGroup "mut_param_syntax"
    [ testCase "0" $
        case replLexparseStmt "fun even(mut x: int) -> bool = true;" of
            Right (Function (Bool, _) (Variable "even" _) [(Int32T, "x", paramToks)] (Multiple [Command (Return (Just _)) _])) ->
                assertBool "param token list should contain mut" (any isMutTok paramToks)
            other -> assertFailure ("expected function with mut parameter, got: " ++ show other)
    ]
    where
        isMutTok :: Token -> Bool
        isMutTok tok = case tok of
            Lex.Ident "mut" _ -> True
            _ -> False

templateFunctionSyntaxTests :: TestTree
templateFunctionSyntaxTests = testGroup "template_function_syntax"
    [ testCase "0" $
        case replLexparseStmt "fun add<T>(a: T, b: T) -> T = a + b;" of
            Right (FunctionT (Class ["T"] _, _) (Variable "add" _) [tpl] params (Multiple [Command (Return (Just _)) _])) -> do
                fst tpl @?= Class ["T"] []
                length params @?= 2
            other -> assertFailure ("expected template function declaration without ::, got: " ++ show other)
    , testCase "tail expr in colon body rewrites to return for typed template function" $
        case replLexparseStmt (unlines
            [ "fun add<T>(a: T, b: T) -> T:"
            , "    a + b"
            ]) of
            Right (FunctionT (Class ["T"] _, _) (Variable "add" _) _ _ (Multiple [Command (Return (Just (Binary Add _ _ _))) _])) ->
                pure ()
            other -> assertFailure ("expected typed template colon-body tail expr rewrite, got: " ++ show other)
    , testCase "tail expr in brace body rewrites to return for typed template function" $
        case replLexparseStmt (unlines
            [ "fun add<T>(a: T, b: T) -> T"
            , "{"
            , "    a + b"
            , "}"
            ]) of
            Right (FunctionT (Class ["T"] _, _) (Variable "add" _) _ _ (Multiple [Command (Return (Just (Binary Add _ _ _))) _])) ->
                pure ()
            other -> assertFailure ("expected typed template brace-body tail expr rewrite, got: " ++ show other)
    ]

untypedFunctionSugarTests :: TestTree
untypedFunctionSugarTests = testGroup "untyped_function_sugar"
    [ testCase "fun add(a, b) lowers to template with excel type params" $
        case replLexparseStmt "fun add(a, b) = a + b;" of
            Right (FunctionT (Class ["A"] _, _) (Variable "add" _) gens params (Multiple [Command (Return (Just _)) _])) -> do
                map fst gens @?= [Class ["A"] [], Class ["B"] [], Class ["C"] []]
                map (\(t, n, _) -> (t, n)) params @?=
                    [ (Class ["B"] [], "a")
                    , (Class ["C"] [], "b")
                    ]
            other -> assertFailure ("expected untyped function sugar to template function, got: " ++ show other)
    , testCase "fun add(mut a, b) keeps mut token on first parameter" $
        case replLexparseStmt "fun add(mut a, b) = a + b;" of
            Right (FunctionT _ (Variable "add" _) _ [(_, "a", toksA), (_, "b", _)] _) ->
                assertBool "first param token list should contain mut" (any isMutTok toksA)
            other -> assertFailure ("expected mut token to survive sugar expansion, got: " ++ show other)
    , testCase "fun add(a, b): tail expr is rewritten to return" $
        case replLexparseStmt (unlines
            [ "fun add(a, b):"
            , "    a + b"
            ]) of
            Right (FunctionT _ (Variable "add" _) _ _ (Multiple [Command (Return (Just (Binary Add _ _ _))) _])) ->
                pure ()
            other -> assertFailure ("expected colon-body tail expr rewrite to return, got: " ++ show other)
    , testCase "fun add(a, b): explicit return remains valid" $
        case replLexparseStmt (unlines
            [ "fun add(a, b):"
            , "    return a + b"
            ]) of
            Right (FunctionT _ (Variable "add" _) _ _ (Multiple [Command (Return (Just (Binary Add _ _ _))) _])) ->
                pure ()
            other -> assertFailure ("expected colon-body explicit return, got: " ++ show other)
    , testCase "fun add(a, b) { tail expr } rewrites to return" $
        case replLexparseStmt (unlines
            [ "fun add(a, b)"
            , "{"
            , "    a + b"
            , "}"
            ]) of
            Right (FunctionT _ (Variable "add" _) _ _ (Multiple [Command (Return (Just (Binary Add _ _ _))) _])) ->
                pure ()
            other -> assertFailure ("expected brace-body tail expr rewrite to return, got: " ++ show other)
    , testCase "fun add(a, b) { return a + b } remains valid" $
        case replLexparseStmt (unlines
            [ "fun add(a, b)"
            , "{"
            , "    return a + b"
            , "}"
            ]) of
            Right (FunctionT _ (Variable "add" _) _ _ (Multiple [Command (Return (Just (Binary Add _ _ _))) _])) ->
                pure ()
            other -> assertFailure ("expected brace-body explicit return, got: " ++ show other)
    , testCase "fun set(arr, size) without value return defaults to void template" $
        case replLexparseStmt (unlines
            [ "fun set(arr, size)"
            , "{"
            , "    var i = 0"
            , "}"
            ]) of
            Right (FunctionT (Void, _) (Variable "set" _) gens params (Multiple _)) -> do
                map fst gens @?= [Class ["A"] [], Class ["B"] []]
                map (\(t, n, _) -> (t, n)) params @?=
                    [ (Class ["A"] [], "arr")
                    , (Class ["B"] [], "size")
                    ]
            other -> assertFailure ("expected void untyped sugar template function, got: " ++ show other)
    ]
  where
    isMutTok :: Token -> Bool
    isMutTok tok = case tok of
        Lex.Ident "mut" _ -> True
        _ -> False

defaultVoidFunctionSyntaxTests :: TestTree
defaultVoidFunctionSyntaxTests = testGroup "default_void_function_syntax"
    [ testCase "fun without return type uses void (colon body)" $
        case replLexparseStmt (unlines
            [ "fun foo(a: int, b: int):"
            , "    return;"
            ]) of
            Right (Function (Void, _) (Variable "foo" _) [(Int32T, "a", _), (Int32T, "b", _)] (Multiple [Command (Return Nothing) _])) ->
                pure ()
            other -> assertFailure ("expected default-void function with colon body, got: " ++ show other)

    , testCase "fun without return type uses void (brace body)" $
        case replLexparseStmt "fun foo(a: int, b: int) {}" of
            Right (Function (Void, _) (Variable "foo" _) [(Int32T, "a", _), (Int32T, "b", _)] (Multiple [])) ->
                pure ()
            other -> assertFailure ("expected default-void function with brace body, got: " ++ show other)

    , testCase "template fun without return type uses void" $
        case replLexparseStmt "fun foo<T>(a: T, b: T) {}" of
            Right (FunctionT (Void, _) (Variable "foo" _) [tpl] [(Class ["T"] _, "a", _), (Class ["T"] _, "b", _)] (Multiple [])) -> do
                fst tpl @?= Class ["T"] []
            other -> assertFailure ("expected default-void template function, got: " ++ show other)
    ]

nativeFunctionSyntaxTests :: TestTree
nativeFunctionSyntaxTests = testGroup "native_function_syntax"
    [ testCase "expr-body native function is lowered to return string" $
        case replLexparseStmt "@native(\"C\") fun add(a: int, b: int) -> int = a + b;" of
            Right (NativeMethod _ _ (Int32T, _) (Variable "add" _) [(Int32T, "a", _), (Int32T, "b", _)] bodyS) ->
                bodyS @?= "return (a+b)"
            other -> assertFailure ("expected NativeMethod with expression body string, got: " ++ show other)

    , testCase "expr-body link function is lowered to return string" $
        case replLexparseStmt "link(\"C\") fun add(a: int, b: int) -> int = a + b;" of
            Right (NativeMethod _ _ (Int32T, _) (Variable "add" _) [(Int32T, "a", _), (Int32T, "b", _)] bodyS) ->
                bodyS @?= "C"
            other -> assertFailure ("expected NativeMethod from link(...) with expression body string, got: " ++ show other)

    , testCase "native block body keeps raw text without braces" $
        case replLexparseProgm "@native(\"C\") fun add(a: int, b: int) -> int {return a & b;}" of
            Right ([], [NativeMethod _ _ (Int32T, _) (Variable "add" _) [(Int32T, "a", _), (Int32T, "b", _)] bodyS]) ->
                bodyS @?= "return a & b;"
            other -> assertFailure ("expected NativeMethod with native raw body, got: " ++ show other)

    , testCase "link modifier order: native inline" $
        case replLexparseStmt "link(\"putln_int\") native inline fun putln(a: int) -> void;" of
            Right (NativeMethod _ _ (Void, _) (Variable "putln" _) [(Int32T, "a", _)] targetS) ->
                targetS @?= "putln_int"
            other -> assertFailure ("expected NativeMethod for link native-inline order, got: " ++ show other)

    , testCase "native modifier order: inline native" $
        case replLexparseStmt "@native(\"putln_int\") inline native fun putln(a: int) -> void;" of
            Right (NativeMethod _ _ (Void, _) (Variable "putln" _) [(Int32T, "a", _)] targetS) ->
                targetS @?= "putln_int"
            other -> assertFailure ("expected NativeMethod for inline native order, got: " ++ show other)

    , testCase "native modifier order: native inline" $
        case replLexparseStmt "@native(\"putln_int\") native inline fun putln(a: int) -> void;" of
            Right (NativeMethod _ _ (Void, _) (Variable "putln" _) [(Int32T, "a", _)] targetS) ->
                targetS @?= "putln_int"
            other -> assertFailure ("expected NativeMethod for native inline order, got: " ++ show other)
    ]

functionPointerTypeSyntaxTests :: TestTree
functionPointerTypeSyntaxTests = testGroup "function_pointer_type_syntax"
    [ testCase "legacy unnamed function pointer type is accepted" $
        case replLexparseStmt "fun sort(src: pointer<*>, comp: (pointer<*>, pointer<*>) -> int) -> void { return; }" of
            Right (Function (Void, _) (Variable "sort" _)
                [ (Pointer Void, "src", _)
                , (FuncPtr Int32T [Pointer Void, Pointer Void], "comp", _)
                ]
                (Multiple [Command (Return Nothing) _])) ->
                    pure ()
            other -> assertFailure ("expected legacy unnamed function pointer parameter type, got: " ++ show other)

    , testCase "named function pointer parameter list remains compatible" $
        case replLexparseStmt "fun sort(src: pointer<*>, comp: (left: pointer<*>, right: pointer<*>) -> int) -> void { return; }" of
            Right (Function (Void, _) (Variable "sort" _)
                [ (Pointer Void, "src", _)
                , (FuncPtr Int32T [Pointer Void, Pointer Void], "comp", _)
                ]
                (Multiple [Command (Return Nothing) _])) ->
                    pure ()
            other -> assertFailure ("expected compatible named function pointer parameter list, got: " ++ show other)
    ]

initAsIdentifierSyntaxTests :: TestTree
initAsIdentifierSyntaxTests = testGroup "init_as_identifier_syntax"
    [ testCase "__init__ is parsed as struct constructor keyword" $
        case replLexparseStmt "struct S { fun __init__(a: int) {} }" of
            Right (Struct _ _ _ [InstanceMethod _ _ (Void, _) (Variable "__init__" _) [(Int32T, "a", _)] (Multiple [])]) ->
                pure ()
            other -> assertFailure ("expected struct constructor named __init__, got: " ++ show other)
    , testCase "init can be used as a normal parameter name and variable reference" $
        case replLexparseStmt "fun fold(init: int) -> int { return init; }" of
            Right (Function (Int32T, _) (Variable "fold" _) [(Int32T, "init", _)] (Multiple [Command (Return (Just (Variable "init" _))) _])) ->
                pure ()
            other -> assertFailure ("expected function using init as param/expr identifier, got: " ++ show other)
    ]

tests :: TestTree
tests = testGroup "Parse.ParseStmt" [
    normalTests,
    valStmtTests,
    typedDeclSyntaxTests,
    loopSyntaxTests,
    ifElifSyntaxTests,
    ifColonNewlineStmtTests,
    returnIfExprTests,
    pointerDerefAssignStmtTests,
    inlineFunctionSyntaxTests,
    mutParamSyntaxTests,
    templateFunctionSyntaxTests,
    untypedFunctionSugarTests,
    defaultVoidFunctionSyntaxTests,
    nativeFunctionSyntaxTests,
    functionPointerTypeSyntaxTests,
    initAsIdentifierSyntaxTests
    ]
