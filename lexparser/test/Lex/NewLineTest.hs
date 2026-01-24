module Lex.NewLineTest where

import Test.Tasty
import Test.Tasty.HUnit

import Lex.Token (Token)
import Lex.Tokenizer
import Lex.NewLine
import Util.Type

import qualified Lex.Token as Lex


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
        setPos (Lex.NewLine _) p = Lex.NewLine p
        setPos (Lex.Ident s _) p = Lex.Ident s p
        setPos (Lex.NumberConst n _) p = Lex.NumberConst n p
        setPos (Lex.StrConst s _) p = Lex.StrConst s p
        setPos (Lex.CharConst c _) p = Lex.CharConst c p
        setPos (Lex.Symbol sym _) p = Lex.Symbol sym p
        setPos (Lex.Error msg _) p = Lex.Error msg p
        setPos (Lex.EOF _) p = Lex.EOF p


banPrevTests :: TestTree
banPrevTests = testGroup "Lex.NewLine.banPrev" $ map (\(n, tok, e) -> testCase n $ banPrev tok @=? e) [
    ("0 in set: LBrace", Lex.Symbol Lex.LBrace (makePosition 1 1 1), True),
    ("1 in set: Semicolon", Lex.Symbol Lex.Semicolon (makePosition 1 2 1), True),
    ("2 not in set: RBrace", Lex.Symbol Lex.RBrace (makePosition 1 3 1), False),
    ("3 non-symbol token", Lex.Ident "x" (makePosition 1 4 1), False)]


banNextTests :: TestTree
banNextTests = testGroup "Lex.NewLine.banNext" $ map (\(n, tok, e) -> testCase n $ banNext tok @=? e) [
    ("0", Lex.Symbol Lex.Assign (makePosition 1 1 1), True),
    ("1", Lex.Symbol Lex.PlusPlus (makePosition 1 2 1), True),
    ("2", Lex.Symbol Lex.At (makePosition 1 3 1), False),
    ("3", Lex.NumberConst "42" (makePosition 1 4 1), False)]


insertNewLineTest :: TestTree
insertNewLineTest = testGroup "Lex.NewLine.insertNewLine" $ map (\(n, s, expected) ->
    testCase n $
        let inputToks = snd (replTokenizeWithNL s)
            actual = replacePos inputToks
        in actual @=? replacePos expected)[
    ("0", unlines [
        "package com.wangdi",
        "class A"], [
            
        ident "package", ident "com", sym Lex.Dot, ident "wangdi",
        nl,
        ident "class", ident "A",
        nl,
        eof]),


    -- Colon is banned on both sides (banNext/banPrev), so we should NOT insert NL around ':'.
    ("1", unlines [
        "class A",
        ":",
        "B"], [
        ident "class", ident "A",
        sym Lex.Colon,
        ident "B",
        nl,
        eof]),
        
    ("2", unlines [
        "if a",
        "b()"], [
        
        ident "if", ident "a",
        nl,
        ident "b", sym Lex.LParen, sym Lex.RParen,
        nl,
        eof]),
        
    ("3", unlines [
        "if a",
        "b()",
        "else",
        "c()"], [
            
        ident "if", ident "a",
        nl,
        ident "b", sym Lex.LParen, sym Lex.RParen,
        nl,
        ident "else",
        nl,
        ident "c", sym Lex.LParen, sym Lex.RParen,
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
        ident "b", sym Lex.LParen, sym Lex.RParen,
        nl,
        ident "else", ident "c", sym Lex.LParen, sym Lex.RParen,
        nl,
        ident "d", sym Lex.LParen, sym Lex.RParen,
        nl,
        eof]),
      
    ("5", unlines [
        "for i",
        "work(i)"], [
            
        ident "for", ident "i",
        nl,
        ident "work", sym Lex.LParen, ident "i", sym Lex.RParen,
        nl,
        eof]),

    ("6", unlines [
        "while cond",
        "work()"], [
            
        ident "while", ident "cond",
        nl,
        ident "work", sym Lex.LParen, sym Lex.RParen,
        nl,
        eof]),

    -- do/while across lines; NL should appear before work and before while.
    ("7", unlines [
        "do",
        "work()",
        "while cond"], [
        
        ident "do",
        nl,
        ident "work", sym Lex.LParen, sym Lex.RParen,
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
        
        ident "do", sym Lex.LBrace,
        ident "work", sym Lex.LParen, sym Lex.RParen,
        sym Lex.RBrace,
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
            
        ident "switch", ident "x", sym Lex.LBrace,
        ident "case", number "1", sym Lex.Colon,
        ident "work", sym Lex.LParen, sym Lex.RParen,
        sym Lex.RBrace,
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
        sym Lex.LParen,
        sym Lex.RParen,
        sym Lex.LBrace,
        ident "return",
        number "1",
        sym Lex.RBrace,
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
            
        ident "int", ident "g", sym Lex.LParen,
        ident "a", sym Lex.Comma,
        ident "b",
        sym Lex.RParen,
        sym Lex.LBrace,
        sym Lex.RBrace,
        nl,
        eof]),

    -- no NL after '{' and no NL before '}' under current block handling.
    ("12", unlines [
        "class A {",
        "int x = 1",
        "}"], [
            
        ident "class", ident "A", sym Lex.LBrace,
        ident "int", ident "x", sym Lex.Assign, number "1",
        sym Lex.RBrace,
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
            
        ident "class", ident "A", sym Lex.LParen, ident "a", sym Lex.RParen,
        sym Lex.Colon,
        ident "B", sym Lex.LParen, sym Lex.RParen,
        sym Lex.LBrace,
        sym Lex.RBrace,
        nl,
        eof]),


    -- No NL right after '{' and no NL right before '}' (current design),
    -- but NL should appear between a() and b() inside the block.
    ("14", unlines [
        "{",
        "a()",
        "b()",
        "}"], [
            
        sym Lex.LBrace,
        ident "a", sym Lex.LParen, sym Lex.RParen,
        nl,
        ident "b", sym Lex.LParen, sym Lex.RParen,
        sym Lex.RBrace,
        nl,
        eof]),
        
    ("15", unlines [
        "{",
        "if a",
        "b()",
        "for i",
        "c()",
        "}"], [
        
        sym Lex.LBrace,
        ident "if", ident "a",
        nl,
        ident "b", sym Lex.LParen, sym Lex.RParen,
        nl,
        ident "for", ident "i",
        nl,
        ident "c", sym Lex.LParen, sym Lex.RParen,
        sym Lex.RBrace,
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
        
        ident "package", ident "com", sym Lex.Dot, ident "wangdi", sym Lex.Dot, ident "collections",
        nl,
        ident "class", ident "List",
        sym Lex.LBrace,
        ident "private", ident "int", sym Lex.LBracket, sym Lex.RBracket, ident "data", sym Lex.Assign,
        ident "int", sym Lex.LBracket, number "10", sym Lex.RBracket,
        nl,
        ident "private", ident "int", ident "length", sym Lex.Assign, number "0",
        nl,
        ident "List", sym Lex.LParen, sym Lex.RParen,
        sym Lex.LBrace,
        ident "println", sym Lex.LParen, str "List created", sym Lex.RParen,
        sym Lex.RBrace,
        nl,
        ident "public", ident "int", ident "getSize", sym Lex.LParen, sym Lex.RParen,
        sym Lex.FatArrow,
        ident "this", sym Lex.Dot, ident "length",
        nl,
        ident "public", ident "int", ident "head", sym Lex.LParen, sym Lex.RParen,
        sym Lex.LBrace,
        ident "if", ident "this", sym Lex.Dot, ident "length", sym Lex.Equal, number "0",
        nl,
        ident "throw", ident "new", ident "Exception", sym Lex.LParen, str "List is empty", sym Lex.RParen,
        nl,
        ident "return", ident "this", sym Lex.Dot, ident "data",
        sym Lex.LBracket, number "0", sym Lex.RBracket,
        sym Lex.RBrace,
        sym Lex.RBrace,
        nl,
        eof])]
    where
        -- Test helpers that ignore real positions (replacePos will normalize anyway).
        z :: Position
        z = makePosition 0 0 0

        ident :: String -> Token
        ident s = Lex.Ident s z

        eof :: Token
        eof = Lex.EOF z

        str :: String -> Token
        str s = Lex.StrConst s z

        number :: String -> Token
        number s = Lex.NumberConst s z

        sym :: Lex.Symbol -> Token
        sym s = Lex.Symbol s z

        nl :: Token
        nl = Lex.NewLine z


tests :: TestTree
tests = testGroup "Lex.NewLineTest" [banPrevTests, banNextTests, insertNewLineTest]
