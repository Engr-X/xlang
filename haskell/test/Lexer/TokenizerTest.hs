module Lexer.TokenizerTest where


import IC.TestHelper

import Data.Trie
import Lexer.Tokenizer
import Control.Monad.State.Strict (evalState)

import qualified Data.Map.Strict as Map
import qualified Util.Exception as UE
import qualified Util.Types as UTypes

-- Basic

symbolMapTest :: [TestCase]
symbolMapTest = [symbolsMap --> Map.fromList [
            ("!", BitReverse), ("!=", NotEqual), ("!^", BitXnor), ("!^=", BitXnorAssign), ("\"", DoubleQuote),
            ("#", Hash),("$", Dollar),("%", Modulo),("%=", ModuloAssign),("&", BitAnd),
            ("'", SingleQuote), ("(", LParen), (")", RParen), ("*", Multiply), ("**", Power),
            ("**=",PowerAssign), ("*/", BlockCommentEnd), ("*=", MultiplyAssign), ("+", Plus), ("++", PlusPlus),
            ("+=", PlusAssign), ("-", Minus), ("--", MinusMinus), ("-=", MinusAssign), (".", Dot), ("..", DoubleDot),
            ("/", Divide), ("/*", BlockCommentStart),("//", LineComment),("/=", DivideAssign), (":", Colon),
            (";", Semicolon), ("<", LessThan), ("<<", BitLShift), ("<<=", BitLShiftAssign), ("<=", LessEqual),
            ("=", Assign), ("==", Equal), (">", GreaterThan), (">=", GreaterEqual), (">>", BitRShift),
            (">>=", BitRShiftAssign), ("?", Question),("?->", QuestionArrow), ("@", At), ("[", LBracket),
            ("\\", Backslash), ("]", RBracket),("^", BitXor),("^=", BitXorAssign),("{", LBrace),
            ("|", BitOr), ("|=", BitOrAssign), ("}", RBrace)]]


symbolsTreeTest :: [TestCase]
symbolsTreeTest = [symbolsTree --> Node False (
    Map.fromList [('!', Node True (Map.fromList [('=', Node True Map.empty), ('^', Node True (Map.fromList [('=', Node True Map.empty)]))])),
    ('"', Node True Map.empty),
    ('#', Node True Map.empty),
    ('$', Node True Map.empty),
    ('%', Node True (Map.fromList [('=', Node True Map.empty)])),
    ('&', Node True Map.empty),
    ('\'', Node True Map.empty),
    ('(', Node True Map.empty),
    (')', Node True Map.empty),
    ('*', Node True (Map.fromList [('*', Node True (Map.fromList [('=', Node True Map.empty)])),
    ('/', Node True Map.empty), ('=', Node True Map.empty)])),
    ('+', Node True (Map.fromList [('+', Node True Map.empty), ('=', Node True Map.empty)])),
    ('-', Node True (Map.fromList [('-', Node True Map.empty), ('=', Node True Map.empty)])),
    ('.', Node True (Map.fromList [('.', Node True Map.empty)])),
    ('/', Node True (Map.fromList [('*', Node True Map.empty), ('/', Node True Map.empty), ('=', Node True Map.empty)])),
    (':', Node True Map.empty),
    (';', Node True Map.empty),
    ('<', Node True (Map.fromList [('<', Node True (Map.fromList [('=', Node True Map.empty)])), ('=', Node True Map.empty)])), ('=', Node True (Map.fromList [('=', Node True Map.empty)])),
    ('>', Node True (Map.fromList [('=', Node True Map.empty), ('>', Node True (Map.fromList [('=', Node True Map.empty)]))])),
    ('?', Node True (Map.fromList [('-', Node False (Map.fromList [('>', Node True Map.empty)]))])),
    ('@', Node True Map.empty),
    ('[', Node True Map.empty),
    ('\\', Node True Map.empty),
    (']', Node True Map.empty),
    ('^', Node True (Map.fromList [('=', Node True Map.empty)])),
    ('{', Node True Map.empty),
    ('|', Node True (Map.fromList [('=', Node True Map.empty)])), ('}', Node True Map.empty)])]


getTokenPosTests :: [TestCase]
getTokenPosTests = map (\(tok, expectedPos) -> getTokenPos tok --> expectedPos) [
    (CharConst 'a' (UTypes.makePosition 1 1 0), UTypes.makePosition 1 1 0),
    (StrConst "hello" (UTypes.makePosition 2 5 0), UTypes.makePosition 2 5 0),
    (NumberConst "123" (UTypes.makePosition 3 2 0), UTypes.makePosition 3 2 0),
    (NumberConst "123L" (UTypes.makePosition 4 4 0), UTypes.makePosition 4 4 0),
    (NumberConst "3.14" (UTypes.makePosition 5 3 0), UTypes.makePosition 5 3 0),
    (NumberConst "2.718" (UTypes.makePosition 6 1 0), UTypes.makePosition 6 1 0),
    (NumberConst "1.0e-10" (UTypes.makePosition 7 7 0), UTypes.makePosition 7 7 0),
    (Ident "x" (UTypes.makePosition 9 5 0), UTypes.makePosition 9 5 0),
    (Symbol Plus (UTypes.makePosition 10 1 0), UTypes.makePosition 10 1 0)]


matchBracketTests :: [TestCase]
matchBracketTests = map (\(x, e) -> matchBracket x --> e) [
    ('(', Just ')'),
    ('[', Just ']'),
    ('{', Just '}'),
    ('}', Nothing),
    (')', Nothing),
    (']', Nothing)]


eatBlockCommentsTests :: [TestCase]
eatBlockCommentsTests = map (\(input, expected) -> eatBlockComments input --> expected) [
    ("/* abc */", "         "),
    ("/* abc */rest",  "         rest"),
    ("x=1;/*comment*/y=2;", "x=1;           y=2;"),
    ("/* a\nb\nc */", "    \n \n    "),
    ("/* ** */ */", "         */"),
    ("abc /* comment */ def", "abc               def"),
    ("/* unterminated", "               "),
    ("/* a /* b */ c */", "             c */"),
    ("no comment here", "no comment here")]


eatSpaceTests :: [TestCase]
eatSpaceTests = map (\(x, expected) -> eatSpace x --> expected) [
    ("   abc", (3, "abc")),
    ("\tabc", (4, "abc")),
    (" \tabc", (5, "abc")),
    ("abc", (0, "abc")),
    ("", (0, "")),
    ("\f\fNext", (2, "Next")),
    (" \t\fend", (6, "end"))]


eatSymbolTests :: [TestCase]
eatSymbolTests = map (\(x, s, r) -> eatSymbol x --> (s, r)) [
    ("==abc", "==", "abc"),
    ("+=xyz", "+=", "xyz"),
    ("!^=123", "!^=", "123"),
    ("++--", "++", "--"),
    ("(foo)", "(", "foo)"),
    ("?->bar", "?->", "bar"),
    ("unknown", "", "unknown"),
    ("=!=!=", "=", "!=!=")]


eatIdentityTests :: [TestCase]
eatIdentityTests = map (\(x, s, r) -> eatIdentity x --> (s, r)) [
    ("variable1 = 10", "variable1", " = 10"),
    ("_hiddenVar + 5", "_hiddenVar", " + 5"),
    ("3invalidStart", "3invalidStart", ""),
    ("valid_name123;", "valid_name123", ";"),
    ("$dollarStart", "", "$dollarStart"),
    ("normalVar!", "normalVar", "!"),
    ("with-dash", "with", "-dash"),
    ("with space", "with", " space"),
    ("", "", "")]


-- | Test cases for 'eatByPattern' with corrected behavior
eatByPatternTests :: [TestCase]
eatByPatternTests = map (\(pat, input, e1, e2) -> eatByPattern pat input --> (e1, e2)) [
    ("^([0-9]+)", "123abc", "123", "abc"),
    ("^([0-9]+)", "abc123", "", "abc123"),
    ("^([0-9]*)", "abc123", "", "abc123"),
    ("^([a-zA-Z]+)", "hello123", "hello", "123"),
    ("^([a-zA-Z]+)", "123hello", "", "123hello"),
    ("^([a-zA-Z0-9]+)", "abc123def", "abc123def", ""),
    ("^(==)", "==x", "==", "x"),
    ("^(==)", "=x", "", "=x")]


eatNumberTests :: [TestCase]
eatNumberTests = map (\(input, p1, p2) -> eatNumber input --> (p1, p2)) [
    ("123abc", "123", "abc"),
    ("+456 rest", "+456", " rest"),
    ("-789xyz", "-789", "xyz"),
    ("0 ", "0", " "),
    ("abc123.3", "", "abc123.3"),
    
    ("123L rest", "123L", " rest"),
    ("456l+", "456l", "+"),
    ("+789L;", "+789L", ";"),
    ("0x1A rest", "0x1A", " rest"),
    ("0XFF;", "0XFF", ";"),
    ("0xabcdef", "0xabcdef", ""),

    ("0x1AL rest", "0x1AL", " rest"),
    ("0XFFl;", "0XFFl", ";"),

    ("1.0 rest", "1.0", " rest"),
    ("3.14159abc", "3.14159", "abc"),
    (".5 rest", ".5", " rest"),
    ("2. rest", "2.", " rest"),

    ("1e10 rest", "1e10", " rest"),
    ("2E+5xyz", "2E+5", "xyz"),
    ("1E-5;", "1E-5", ";"),
    
    ("1.0e10 rest", "1.0e10", " rest"),
    ("3.14e+2x", "3.14e+2", "x"),
    ("0.5E-3;", "0.5E-3", ";"),

    ("+.5e3 ", "+.5e3", " "),
    ("-0.5E2,", "-0.5E2", ","),
    (".5e3 ", ".5e3", " "),
    
    ("1.0f rest", "1.0f", " rest"),
    ("3.14F;", "3.14F", ";"),
    ("1e3f ", "1e3f", " "),
    
    ("1.0L rest", "1.0L", " rest"),
    ("3.14l;", "3.14l", ";"),
    ("1e3L ", "1e3L", " "),
    
    ("", "", ""),
    ("abc", "", "abc"),
    ("xyz123", "", "xyz123"),
    ("+", "", "+"),
    (".", "", "."),

    ("123.456e+7f rest", "123.456e+7f", " rest"),
    ("0x1A2B3CL;", "0x1A2B3CL", ";"),
    ("+0.0 ", "+0.0", " "),
    ("-.5e-3L,", "-.5e-3L", ",")]


eatStringLiteralTests :: [TestCase]
eatStringLiteralTests = map (\(input, expected) -> eatStringLiteral input --> expected) [
    ("\"hello\" rest", Just ("\"hello\"", " rest")),
    ("\"world\";", Just ("\"world\"", ";")),
    ("\"test\"", Just ("\"test\"", "")),
    
    ("\"\" rest", Just ("\"\"", " rest")),
    ("\"\"", Just ("\"\"", "")),
    
    ("\"hello\\\"world\" rest", Just ("\"hello\\\"world\"", " rest")),
    ("\"say \\\"hi\\\"\" rest", Just ("\"say \\\"hi\\\"\"", " rest")),
    
    ("\"path\\\\to\\\\file\" rest", Just ("\"path\\\\to\\\\file\"", " rest")),
    ("\"end\\\\\" rest", Just ("\"end\\\\\"", " rest")),
    
    ("\"line1\\nline2\" rest", Just ("\"line1\\nline2\"", " rest")),
    ("\"tab\\there\" rest", Just ("\"tab\\there\"", " rest")),
    ("\"null\\0char\" rest", Just ("\"null\\0char\"", " rest")),
    ("\"mixed\\\"\\\\\\n\" rest", Just ("\"mixed\\\"\\\\\\n\"", " rest")),
    
    ("\"hello world\" rest", Just ("\"hello world\"", " rest")),
    ("\"123 + 456\" rest", Just ("\"123 + 456\"", " rest")),
    ("\"a@b#c$d\" rest", Just ("\"a@b#c$d\"", " rest")),
    
    ("\"你好世界\" rest", Just ("\"你好世界\"", " rest")),
    ("\"émoji 😀\" rest", Just ("\"émoji 😀\"", " rest")),
    
    ("", Nothing), 
    ("hello", Nothing),
    ("\"unclosed", Nothing),
    ("\"missing", Nothing),
    
    ("\"first\"\"second\"", Just ("\"first\"", "\"second\"")),
    ("\"a\"\"b\"\"c\"", Just ("\"a\"", "\"b\"\"c\"")),
    
    ("\"\\\"\" rest", Just ("\"\\\"\"", " rest")),
    ("\"\\\\\" rest", Just ("\"\\\\\"", " rest")),
    ("\"\\\"\\\"\" rest", Just ("\"\\\"\\\"\"", " rest")),
    
    ("\"hello\", other", Just ("\"hello\"", ", other")),
    ("\"test\");", Just ("\"test\"", ");")),
    ("\"value\" + \"other\"", Just ("\"value\"", " + \"other\""))]


eatCharLiteralTests :: [TestCase]
eatCharLiteralTests = map (\(input, expected) -> eatCharLiteral input --> expected) [
    ("'a' rest", Just ("'a'", " rest")),
    ("'Z';", Just ("'Z'", ";")),
    ("'0'", Just ("'0'", "")),
    ("'_' rest", Just ("'_'", " rest")),

    ("'\\n' rest", Just ("'\\n'", " rest")),
    ("'\\t' rest", Just ("'\\t'", " rest")),
    ("'\\r' rest", Just ("'\\r'", " rest")),
    ("'\\0' rest", Just ("'\\0'", " rest")),

    ("'\\'' rest", Just ("'\\''", " rest")),
    ("'\\\\' rest", Just ("'\\\\'", " rest")),

    ("'\\x41' rest", Just ("'\\x41'", " rest")),
    ("'\\101' rest", Just ("'\\101'", " rest")),

    ("'你' rest", Just ("'你'", " rest")),
    ("'é' rest", Just ("'é'", " rest")),
    ("'😀' rest", Just ("'😀'", " rest")),

    ("'a''b'", Just ("'a'", "'b'")),
    ("'x''y''z'", Just ("'x'", "'y''z'")),

    ("", Nothing),
    ("abc", Nothing),

    ("'' rest", Nothing),
    ("'ab' rest", Nothing),
    ("'a", Nothing),
    ("'\\' rest", Nothing),
    ("'\\x' rest", Nothing),
    ("'\\xZZ' rest", Nothing),

    ("'a', next", Just ("'a'", ", next")),
    ("'b');", Just ("'b'", ");"))]


unwrapStringTests :: [TestCase]
unwrapStringTests = map (\(input, expected) -> unwrapString input --> expected) [
    ("\"hello\"", "hello"),
    ("\"world\"", "world"),
    ("\"\"", ""),
    ("\"123 + 456\"", "123 + 456"),
    ("\"a@b#c$d\"", "a@b#c$d"),
    ("\"\"你好世界\"", "\"你好世界")]


unwrapCharTests :: [TestCase]
unwrapCharTests = map (\(input, expected) -> unwrapChar input --> expected) [
    ("'a'", 'a'), ("'Z'", 'Z'), ("'0'", '0'), ("'+'", '+'),
    ("'你'", '你'), ("'书'", '书'), ("'界'", '界'),
    ("'\\n'", '\n'), ("'\\t'", '\t'), ("'\\r'", '\r'), ("'\\b'", '\b'), ("'\\f'", '\f'), ("'\\v'", '\v'),
    ("'\\\\'", '\\'), ("'\\''", '\''), ("'\\\"'", '\"'),
    ("'\\0'", '\0'), ("'\\12'", '\n'), ("'\\256'", '\174'),
    ("'\\x41'", 'A'), ("'\\x7a'", 'z'), ("'\\u4E66'", '书'), ("'\\u4F60'", '你'), ("'\\U00004E16'", '世')]


parseSymbolTests :: [TestCase]
parseSymbolTests =  map (\(input, expected) -> parseSymbol input --> expected) [
    ("=", Just Assign), ("<<=", Just BitLShiftAssign), ("@", Just At), ("~", Nothing)]


defaultPath :: UTypes.Path
defaultPath = "default path"


getTokenizerState :: [String] -> TokenizerState
getTokenizerState = makeState defaultPath (UTypes.makePosition 1 1 0)


eatTests :: [TestCase]
eatTests = map (\(ls, expected) -> let st = getTokenizerState ls in evalState eat st --> expected) [
    ([""], Right End), (["   "], Right End),
    (["a"], Right $ Ident "a" (UTypes.makePosition 1 1 1)), (["  _abc"], Right $ Ident "_abc" (UTypes.makePosition 1 3 4)),
    (["   // do som", "  123"], Right $ NumberConst "123" (UTypes.makePosition 2 3 3)),
    (["+123 + 1234"], Right $ NumberConst "+123" (UTypes.makePosition 1 1 4)),
    ([".123456aaa"], Right $ NumberConst ".123456" (UTypes.makePosition 1 1 7)),
    ([".function"], Right $ Symbol Dot (UTypes.makePosition 1 1 1)), 
    ([" /* dosomething"], Left $ UE.LexerError $ UE.makeError defaultPath (UTypes.makePosition 1 2 2) UE.unclosedCommentMsg),
    (["// do", "// aaa", "var a = 1"], Right $ Ident "var" (UTypes.makePosition 3 1 3))]


tests :: [TestGroup]
tests = [
    testGroup "Lexer.Tokenizer.symbolMap" symbolMapTest,
    testGroup "Lexer.Tokenizer.symbolsTree" symbolsTreeTest,
    testGroup "Lexer.Tokenizer.getTokenPos" getTokenPosTests,

    testGroup "Lexer.Tokenizer.matchBracket" matchBracketTests,
    testGroup "Lexer.Tokenizer.eatBlockComments" eatBlockCommentsTests,
    testGroup "Lexer.Tokenizer.eatSpace" eatSpaceTests,
    testGroup "Lexer.Tokenizer.eatSymbol" eatSymbolTests,
    testGroup "Lexer.Tokenizer.eatIdentity" eatIdentityTests,
    testGroup "Lexer.Tokenizer.eatByPattern" eatByPatternTests,
    testGroup "Lexer.Tokenizer.eatNumber" eatNumberTests,
    testGroup "Lexer.Tokenizer.eatStringLiteral" eatStringLiteralTests,
    testGroup "Lexer.Tokenizer.eatCharLiteral" eatCharLiteralTests,
    
    testGroup "Lexer.Tokenizer.unwrapString" unwrapStringTests,
    testGroup "Lexer.Tokenizer.unwrapChar" unwrapCharTests,
    
    testGroup "Lexer.Tokenizer.parseSymbol" parseSymbolTests,
    testGroup "Lexer.Tokenizer.eat" eatTests]