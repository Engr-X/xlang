module Lexer.TokenizerTest where


import IC.TestHelper

import qualified Lexer.Tokenizer as LT

matchBracketTests :: [TestCase]
matchBracketTests = map (\(x, e) -> LT.matchBracket x --> e) [
    ('(', Just ')'),
    ('[', Just ']'),
    ('{', Just '}'),
    ('}', Nothing),
    (')', Nothing),
    (']', Nothing)]

eatBlockCommentsTests :: [TestCase]
eatBlockCommentsTests = map (\(input, expected) -> LT.eatBlockComments input --> expected) [
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
eatSpaceTests = map (\(x, expected) -> LT.eatSpace x --> expected) [
    ("   abc", (3, "abc")),
    ("\tabc", (4, "abc")),
    (" \tabc", (5, "abc")),
    ("abc", (0, "abc")),
    ("", (0, "")),
    ("\f\fNext", (2, "Next")),
    (" \t\fend", (6, "end"))]

eatSymbolTests :: [TestCase]
eatSymbolTests = map (\(x, s, r) -> LT.eatSymbol x --> (s, r)) [
    ("==abc", "==", "abc"),
    ("+=xyz", "+=", "xyz"),
    ("!^=123", "!^=", "123"),
    ("++--", "++", "--"),
    ("(foo)", "(", "foo)"),
    ("?->bar", "?->", "bar"),
    ("unknown", "", "unknown"),
    ("=!=!=", "=", "!=!=")]

eatIdentityTests :: [TestCase]
eatIdentityTests = map (\(x, s, r) -> LT.eatIdentity x --> (s, r)) [
    ("variable1 = 10", "variable1", " = 10"),
    ("_hiddenVar + 5", "_hiddenVar", " + 5"),
    ("3invalidStart", "3invalidStart", ""),
    ("valid_name123;", "valid_name123", ";"),
    ("$dollarStart", "", "$dollarStart"),
    ("normalVar!", "normalVar", "!"),
    ("with-dash", "with", "-dash"),
    ("with space", "with", " space"),
    ("", "", "")]

eatNumberTests :: [TestCase]
eatNumberTests = map (\(input, p1, p2) -> LT.eatNumber input --> (p1, p2)) [
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
eatStringLiteralTests = map (\(input, expected) -> LT.eatStringLiteral input --> expected) [
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
eatCharLiteralTests = map (\(input, expected) -> LT.eatCharLiteral input --> expected) [
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

tests :: [TestGroup]
tests = [
    testGroup "Lexer.Tokenizer.matchBracket" matchBracketTests,
    testGroup "Lexer.Tokenizer.eatBlockComments" eatBlockCommentsTests,
    testGroup "Lexer.Tokenizer.eatSpace" eatSpaceTests,
    testGroup "Lexer.Tokenizer.eatSymbol" eatSymbolTests,
    testGroup "Lexer.Tokenizer.eatIdentity" eatIdentityTests,
    testGroup "Lexer.Tokenizer.eatNumber" eatNumberTests,
    testGroup "Lexer.Tokenizer.eatStringLiteral" eatStringLiteralTests,
    testGroup "Lexer.Tokenizer.eatCharLiteral" eatCharLiteralTests]