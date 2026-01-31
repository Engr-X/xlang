{-# LANGUAGE OverloadedStrings #-}

module Lex.TokenPassTest where

import Test.Tasty
import Test.Tasty.HUnit

import Lex.Token
import Lex.Tokenizer
import Lex.TokenPass 
import Util.Type


replacePos :: [Token] -> [Token]
replacePos = go []
    where
        -- Normalize all token positions to zero to make comparisons stable.
        zeroPos :: Position
        zeroPos = makePosition 0 0 0

        go :: [Token] -> [Token] -> [Token]
        go acc [] = reverse acc
        go acc (tok : ts) = go (setPos tok zeroPos : acc) ts

        setPos :: Token -> Position -> Token
        setPos (TokenPass  _) p = TokenPass  p
        setPos (Ident s _) p = Ident s p
        setPos (NumberConst n _) p = NumberConst n p
        setPos (StrConst s _) p = StrConst s p
        setPos (CharConst c _) p = CharConst c p
        setPos (Symbol sym _) p = Symbol sym p
        setPos (Error msg _) p = Error msg p
        setPos (EOF _) p = EOF p


banPrevTests :: TestTree
banPrevTests = testGroup "Lex.TokenPass.banPrev" $ map (\(n, tok, e) -> testCase n $ banPrev tok @=? e) [
    ("0", Symbol LBrace (makePosition 1 1 1), True),
    ("1", Symbol Semicolon (makePosition 1 2 1), True),
    ("2", Symbol RBrace (makePosition 1 3 1), False),
    ("3", Ident "x" (makePosition 1 4 1), False)]


banNextTests :: TestTree
banNextTests = testGroup "Lex.TokenPass.banNext" $ map (\(n, tok, e) -> testCase n $ banNext tok @=? e) [
    ("0", Symbol Assign (makePosition 1 1 1), True),
    ("1", Symbol PlusPlus (makePosition 1 2 1), False),
    ("2", Symbol At (makePosition 1 3 1), False),
    ("3", NumberConst "42" (makePosition 1 4 1), False)]


insertTokenPassTest :: TestTree
insertTokenPassTest = testGroup "Lex.TokenPass.insertTokenPass " $ map (\(n, s, expected) ->
    testCase n $
        let inputToks = snd (replTokenizeWithNL s)
            actual = replacePos inputToks
        in actual @=? replacePos expected)[
    ("0", unlines [
        "package com.wangdi",
        "class A"], [
            
        ident "package", ident "com", sym Dot, ident "wangdi",
        nl,
        ident "class", ident "A",
        nl,
        eof]),

    ("1", unlines [
        "class A",
        ":",
        "B"], [
        ident "class", ident "A",
        sym Colon,
        ident "B",
        nl,
        eof]),
        
    ("2", unlines [
        "if a",
        "b()"], [
        
        ident "if", ident "a",
        nl,
        ident "b", sym LParen, sym RParen,
        nl,
        eof]),
        
    ("3", unlines [
        "if a",
        "b()",
        "else",
        "c()"], [
            
        ident "if", ident "a",
        nl,
        ident "b", sym LParen, sym RParen,
        nl,
        ident "else",
        nl,
        ident "c", sym LParen, sym RParen,
        nl,
        eof]),

    -- else followed by statement on same line; only NL between lines.
    ("4", unlines [
        "if a",
        "b()",
        "else c()",
        "d()"], [
        
        ident "if", ident "a",
        nl,
        ident "b", sym LParen, sym RParen,
        nl,
        ident "else", ident "c", sym LParen, sym RParen,
        nl,
        ident "d", sym LParen, sym RParen,
        nl,
        eof]),
      
    ("5", unlines [
        "for i",
        "work(i)"], [
            
        ident "for", ident "i",
        nl,
        ident "work", sym LParen, ident "i", sym RParen,
        nl,
        eof]),

    ("6", unlines [
        "while cond",
        "work()"], [
            
        ident "while", ident "cond",
        nl,
        ident "work", sym LParen, sym RParen,
        nl,
        eof]),

    -- do/while across lines; NL should appear before work and before while.
    ("7", unlines [
        "do",
        "work()",
        "while cond"], [
        
        ident "do",
        nl,
        ident "work", sym LParen, sym RParen,
        nl,
        ident "while", ident "cond",
        nl,
        eof]),


    -- Current implementation does NOT insert NL immediately after '{' (block-first-token),
    -- because inner recursion starts with prev = Nothing.
    ("8", unlines [
        "do {",
        "work()",
        "}",
        "while cond"], [
        
        ident "do", sym LBrace,
        ident "work", sym LParen, sym RParen,
        nl,
        sym RBrace,
        nl,
        ident "while", ident "cond",
        nl,
        eof]),


    -- No NL after '{' and no NL after ':' because ':' is banned on both sides.
    ("9", unlines [
        "switch x {",
        "case 1:",
        "work()",
        "}"], [
            
        ident "switch", ident "x", sym LBrace,
        ident "case", number "1", sym Colon,
        ident "work", sym LParen, sym RParen,
        nl,
        sym RBrace,
        nl,
        eof]),


    -- NL is inserted before '{' (line change), but not inserted after '{' or before '}'.
    ("10", unlines [
        "int f()",
        "{",
        "return 1",
        "}"], [
            
        ident "int",
        ident "f",
        sym LParen,
        sym RParen,
        sym LBrace,
        ident "return",
        number "1",
        nl,
        sym RBrace,
        nl,
        eof]),


    -- parenDepth suppresses NL inside () even across lines.
    -- We still expect NL before '{' due to line change after ')'.
    ("11", unlines [
        "int g(",
        "a,",
        "b",
        ")",
        "{",
        "}"], [
            
        ident "int", ident "g", sym LParen,
        ident "a", sym Comma,
        ident "b",
        sym RParen,
        sym LBrace,
        nl,
        sym RBrace,
        nl,
        eof]),

    -- no NL after '{' and no NL before '}' under current block handling.
    ("12", unlines [
        "class A {",
        "int x = 1",
        "}"], [
            
        ident "class", ident "A", sym LBrace,
        ident "int", ident "x", sym Assign, number "1",
        nl,
        sym RBrace,
        nl,
        eof]),


    -- ':' is banned on both sides, so no NL around it.
    -- NL is inserted before '{' because previous token ')' is on a different line.
    ( "13", unlines [
        "class A(a)",
        ":",
        "B()",
        "{",
        "}"], [
            
        ident "class", ident "A", sym LParen, ident "a", sym RParen,
        sym Colon,
        ident "B", sym LParen, sym RParen,
        sym LBrace,
        nl,
        sym RBrace,
        nl,
        eof]),


    -- No NL right after '{' and no NL right before '}' (current design),
    -- but NL should appear between a() and b() inside the block.
    ("14", unlines [
        "{",
        "a()",
        "b()",
        "}"], [
            
        sym LBrace,
        ident "a", sym LParen, sym RParen,
        nl,
        ident "b", sym LParen, sym RParen,
        nl,
        sym RBrace,
        nl,
        eof]),
        
    ("15", unlines [
        "{",
        "if a",
        "b()",
        "for i",
        "c()",
        "}"], [
        
        sym LBrace,
        ident "if", ident "a",
        nl,
        ident "b", sym LParen, sym RParen,
        nl,
        ident "for", ident "i",
        nl,
        ident "c", sym LParen, sym RParen,
        nl,
        sym RBrace,
        nl,
        eof]),
        
    ( "16", unlines [
        "package com.wangdi.collections",
        "",
        "class List",
        "{",
        "    private int[] data = int[10]",
        "",
        "    private int length = 0",
        "",
        "    List()",
        "    {",
        "        println(\"List created\")",
        "    }",
        "",
        "    public int getSize() => this.length",
        "",
        "    public int head()",
        "    {",
        "        if this.length == 0",
        "            throw new Exception(\"List is empty\")",
        "        ",
        "        return this.data[0]",
        "    }",
        "}"], [
        
        ident "package", ident "com", sym Dot, ident "wangdi", sym Dot, ident "collections",
        nl,
        ident "class", ident "List",
        sym LBrace,
        ident "private", ident "int", sym LBracket, sym RBracket, ident "data", sym Assign,
        ident "int", sym LBracket, number "10", sym RBracket,
        nl,
        ident "private", ident "int", ident "length", sym Assign, number "0",
        nl,
        ident "List", sym LParen, sym RParen,
        sym LBrace,
        ident "println", sym LParen, str "List created", sym RParen,
        nl,
        sym RBrace,
        nl,
        ident "public", ident "int", ident "getSize", sym LParen, sym RParen,
        sym FatArrow,
        ident "this", sym Dot, ident "length",
        nl,
        ident "public", ident "int", ident "head", sym LParen, sym RParen,
        sym LBrace,
        ident "if", ident "this", sym Dot, ident "length", sym Equal, number "0",
        nl,
        ident "throw", ident "new", ident "Exception", sym LParen, str "List is empty", sym RParen,
        nl,
        ident "return", ident "this", sym Dot, ident "data",
        sym LBracket, number "0", sym RBracket,
        nl,
        sym RBrace,
        nl,
        sym RBrace,
        nl,
        eof])]
    where
        -- Test helpers that ignore real positions (replacePos will normalize anyway).
        z :: Position
        z = makePosition 0 0 0

        ident :: String -> Token
        ident s = Ident s z

        eof :: Token
        eof = EOF z

        str :: String -> Token
        str s = StrConst s z

        number :: String -> Token
        number s = NumberConst s z

        sym :: Symbol -> Token
        sym s = Symbol s z

        nl :: Token
        nl = TokenPass  z



-- English comment: Strip EOF for stable comparisons across lexer details.
stripEOF :: [Token] -> [Token]
stripEOF = filter (not . isEOF)

mkSym :: Symbol -> Int -> Int -> Int -> Token
mkSym s a b c = Symbol s $ Position a b c

mkNum :: String -> Int -> Int -> Int -> Token
mkNum s a b c = NumberConst s $ Position a b c

mkId :: String -> Int -> Int -> Int -> Token
mkId s a b c = Ident s $ makePosition a b c

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
splitShiftInGenericsTests :: TestTree
splitShiftInGenericsTests = testGroup "Lex.TokenPass.splitShiftInGenerics" $
    map (\(n, input, expected) ->
        testCase n $
            let (_errs, toks) = tokenize ("stdin" :: Path) input
            in stripEOF toks @=? expected) [
    ("0", "a >> b", [
        mkId  "a" 1 1 1,
        mkSym BitRShift 1 3 2,
        mkId  "b" 1 6 1]),

    ("1", "foo::<HashMap::<List::<Int>>>(h)", [
        mkId  "foo" 1 1 3,
        mkSym DoubleColon 1 4 2,
        mkSym LessThan 1 6 1,
        mkId "HashMap" 1 7 7,
        mkSym DoubleColon 1 14 2,
        mkSym LessThan 1 16 1,
        mkId  "List" 1 17 4,
        mkSym DoubleColon 1 21 2,
        mkSym LessThan 1 23 1,
        mkId "Int" 1 24 3,

        mkSym GreaterThan 1 27 1,
        mkSym GreaterThan 1 28 1,
        mkSym GreaterThan 1 29 1,
        mkSym LParen 1 30 1,
        mkId "h" 1 31 1,
        mkSym RParen 1 32 1]),

    ("2", "f::<List::<Int>>(x)", [
        mkId "f" 1 1 1,
        mkSym DoubleColon 1 2 2,
        mkSym LessThan 1 4 1,
        mkId "List" 1 5 4,
        mkSym DoubleColon 1 9 2,
        mkSym LessThan 1 11 1,
        mkId "Int" 1 12 3,
        mkSym GreaterThan 1 15 1,
        mkSym GreaterThan 1 16 1,
        mkSym LParen 1 17 1,
        mkId "x" 1 18 1,
        mkSym RParen 1 19 1]),

    ("3", "g::<Int>(a) >> 1", [
        mkId "g" 1 1 1,
        mkSym DoubleColon 1 2 2,
        mkSym LessThan 1 4 1,
        mkId "Int" 1 5 3,
        mkSym GreaterThan 1 8 1,
        mkSym LParen 1 9 1,
        mkId "a" 1 10 1,
        mkSym RParen 1 11 1,

        -- English comment: outside generic context, keep shift.
        mkSym BitRShift 1 13 2,

        mkNum "1" 1 16 1])]


tests :: TestTree
tests = testGroup "Lex.TokenPassTest" [banPrevTests, banNextTests, insertTokenPassTest, splitShiftInGenericsTests]
