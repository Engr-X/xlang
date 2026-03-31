module Parse.ParseStmtTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseStmt
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
    [ testCase "int_a" $
        case replLexparseStmt "int a;" of
            Right (DefField ["a"] (Just _) Nothing _) -> pure ()
            other -> assertFailure ("expected typed DefField, got: " ++ show other)
    , testCase "const_int_a_eq_10" $
        case replLexparseStmt "const int a = 10;" of
            Right (DefConstField ["a"] (Just _) (Just _) _) -> pure ()
            other -> assertFailure ("expected typed DefConstField with init, got: " ++ show other)
    , testCase "final_int_a_eq_10" $
        case replLexparseStmt "final int a = 10;" of
            Right (DefConstField ["a"] (Just _) (Just _) _) -> pure ()
            other -> assertFailure ("expected typed DefConstField with init, got: " ++ show other)
    , testCase "var_multi_mixed_types" $
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
    , testCase "c_like_multi_same_type" $
        case replLexparseStmt "int a = 10, b = 10;" of
            Right (StmtGroup
                [ DefField ["a"] (Just Int32T) (Just (IntConst "10" _)) _
                , DefField ["b"] (Just Int32T) (Just (IntConst "10" _)) _
                ]) -> pure ()
            other -> assertFailure ("expected c-like multi typed decl block, got: " ++ show other)
    ]

loopSyntaxTests :: TestTree
loopSyntaxTests = testGroup "loop_syntax"
    [ testCase "until_else_block" $
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

inlineFunctionSyntaxTests :: TestTree
inlineFunctionSyntaxTests = testGroup "inline_function_syntax"
    [ testCase "inline fun parses and keeps inline modifier token" $
        case replLexparseStmt "inline fun add(a: int, b: int) -> int = a + b;" of
            Right (Function (Int32T, retToks) (Variable "add" _) _ (Multiple [Command (Return (Just _)) _])) ->
                assertBool "inline token should exist in return-token list" (hasInlineTok retToks)
            other -> assertFailure ("expected inline function statement, got: " ++ show other)
    , testCase "inline var is rejected (inline is function-only)" $
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
    [ testCase "fun param supports mut x: int" $
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
    [ testCase "fun add<T>(...) parses without :: in declaration" $
        case replLexparseStmt "fun add<T>(a: T, b: T) -> T = a + b;" of
            Right (FunctionT (Class ["T"] _, _) (Variable "add" _) [tpl] params (Multiple [Command (Return (Just _)) _])) -> do
                fst tpl @?= Class ["T"] []
                length params @?= 2
            other -> assertFailure ("expected template function declaration without ::, got: " ++ show other)
    ]

tests :: TestTree
tests = testGroup "Parse.ParseStmt" [
    normalTests,
    valStmtTests,
    typedDeclSyntaxTests,
    loopSyntaxTests,
    ifElifSyntaxTests,
    inlineFunctionSyntaxTests,
    mutParamSyntaxTests,
    templateFunctionSyntaxTests
    ]
