@file.class("Tokenizer")
package xlang.compiler.lexer


import xlang.System
import xlang.lexer.Lex
import xlang.lexer.LexPosition
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.lexer.TokenizeFSM
import xlang.lexer.TokenizeRule
import xlang.util.string.String
import xlang.util.string.StringBuilder


val INVALID_TOKEN_ERROR_MSG: pointer<char> = "invalid token: "
val UNTERMINATED_BLOCK_COMMENT_ERROR_MSG: pointer<char> = "unterminated block comment"
val UNTERMINATED_CHAR_ERROR_MSG: pointer<char> = "unterminated char literal"
val UNTERMINATED_STRING_ERROR_MSG: pointer<char> = "unterminated string literal"
val INVALID_IDENTITY_NAME_ERROR_MSG: pointer<char> = "invalid identity name: "
val LINE_COMMENT_STATE: int = 10
val BLOCK_COMMENT_STATE: int = 11
val CHAR_STATE: int = 12
val STRING_STATE: int = 13
val TK_LINE_TERMINATOR: int = 1
val TK_IDENTITY: int = 2
val TK_INTEGER: int = 3
val TK_LONG: int = 4
val TK_FLOAT: int = 5
val TK_DOUBLE: int = 6
val TK_LONG_DOUBLE: int = 7
val TK_CHAR: int = 8
val TK_STRING: int = 9


val tokenDefStart: int = 100
val KW_AND: int = 100
val KW_AS: int = 101
val KW_BLOB: int = 102
val KW_BOOL: int = 103
val KW_BREAK: int = 104
val KW_BYTE: int = 105
val KW_CHAR: int = 106
val KW_CLASS: int = 107
val KW_CONTINUE: int = 108
val KW_DO: int = 109
val KW_DOUBLE: int = 110
val KW_ELIF: int = 111
val KW_ELSE: int = 112
val KW_FALSE: int = 113
val KW_FLOAT: int = 114
val KW_FOR: int = 115
val KW_FUN: int = 116
val KW_IF: int = 117
val KW_IMPLIES: int = 118
val KW_IMPORT: int = 119
val KW_INLINE: int = 120
val KW_INT: int = 121
val KW_INV: int = 122
val KW_LONG: int = 123
val KW_LOOP: int = 124
val KW_MUT: int = 125
val KW_NAND: int = 126
val KW_NATIVE: int = 127
val KW_NEW: int = 128
val KW_NIMPLIES: int = 129
val KW_NOR: int = 130
val KW_NULL: int = 131
val KW_OR: int = 132
val KW_PACKAGE: int = 133
val KW_PASS: int = 134
val KW_POINTER: int = 135
val KW_PRIVATE: int = 136
val KW_PROTECTED: int = 137
val KW_PUBLIC: int = 138
val KW_REPEAT: int = 139
val KW_RETURN: int = 140
val KW_SHL: int = 141
val KW_SHORT: int = 142
val KW_SHR: int = 143
val KW_STATIC: int = 144
val KW_STRUCT: int = 145
val KW_TRUE: int = 146
val KW_UNTIL: int = 147
val KW_USHR: int = 148
val KW_VAL: int = 149
val KW_VAR: int = 150
val KW_WHILE: int = 151
val KW_XNOR: int = 152
val KW_XOR: int = 153


val keywordListLength: int = 54
val keywordListSlotSize: int = 16
val keywordTextSpace: blob[864 * sizeof(char)]
val keywordTextList: pointer<char> = keywordTextSpace as pointer<char>


private inline fun getKeywordText(index: int) -> pointer<char> =
    keywordTextList + index * keywordListSlotSize


private fun keywordListInit()
{
    String.strcpy(getKeywordText(0), "and")
    String.strcpy(getKeywordText(1), "as")
    String.strcpy(getKeywordText(2), "blob")
    String.strcpy(getKeywordText(3), "bool")
    String.strcpy(getKeywordText(4), "break")
    String.strcpy(getKeywordText(5), "byte")
    String.strcpy(getKeywordText(6), "char")
    String.strcpy(getKeywordText(7), "class")
    String.strcpy(getKeywordText(8), "continue")
    String.strcpy(getKeywordText(9), "do")
    String.strcpy(getKeywordText(10), "double")
    String.strcpy(getKeywordText(11), "elif")
    String.strcpy(getKeywordText(12), "else")
    String.strcpy(getKeywordText(13), "false")
    String.strcpy(getKeywordText(14), "float")
    String.strcpy(getKeywordText(15), "for")
    String.strcpy(getKeywordText(16), "fun")
    String.strcpy(getKeywordText(17), "if")
    String.strcpy(getKeywordText(18), "implies")
    String.strcpy(getKeywordText(19), "import")
    String.strcpy(getKeywordText(20), "inline")
    String.strcpy(getKeywordText(21), "int")
    String.strcpy(getKeywordText(22), "inv")
    String.strcpy(getKeywordText(23), "long")
    String.strcpy(getKeywordText(24), "loop")
    String.strcpy(getKeywordText(25), "mut")
    String.strcpy(getKeywordText(26), "nand")
    String.strcpy(getKeywordText(27), "native")
    String.strcpy(getKeywordText(28), "new")
    String.strcpy(getKeywordText(29), "nimplies")
    String.strcpy(getKeywordText(30), "nor")
    String.strcpy(getKeywordText(31), "null")
    String.strcpy(getKeywordText(32), "or")
    String.strcpy(getKeywordText(33), "package")
    String.strcpy(getKeywordText(34), "pass")
    String.strcpy(getKeywordText(35), "pointer")
    String.strcpy(getKeywordText(36), "private")
    String.strcpy(getKeywordText(37), "protected")
    String.strcpy(getKeywordText(38), "public")
    String.strcpy(getKeywordText(39), "repeat")
    String.strcpy(getKeywordText(40), "return")
    String.strcpy(getKeywordText(41), "shl")
    String.strcpy(getKeywordText(42), "short")
    String.strcpy(getKeywordText(43), "shr")
    String.strcpy(getKeywordText(44), "static")
    String.strcpy(getKeywordText(45), "struct")
    String.strcpy(getKeywordText(46), "true")
    String.strcpy(getKeywordText(47), "until")
    String.strcpy(getKeywordText(48), "ushr")
    String.strcpy(getKeywordText(49), "val")
    String.strcpy(getKeywordText(50), "var")
    String.strcpy(getKeywordText(51), "while")
    String.strcpy(getKeywordText(52), "xnor")
    String.strcpy(getKeywordText(53), "xor")
}


val symbolDefStart: int = 1000
val DOUBLE_LESS_EQUAL: int = 1000
val DOUBLE_GREATER_EQUAL: int = 1001
val BANG_CARET_EQUAL: int = 1002
val DOUBLE_STAR_EQUAL: int = 1003
val QUESTION_ARROW: int = 1004
val BANG_DOUBLE_AMPERSAND: int = 1005
val BANG_DOUBLE_PIPE: int = 1006
val NOT_ARROW: int = 1007
val DOUBLE_ARROW: int = 1008
val TRIPLE_EQUAL: int = 1009
val PIPE_EQUAL: int = 1010
val CARET_EQUAL: int = 1011
val PLUS_EQUAL: int = 1012
val MINUS_EQUAL: int = 1013
val STAR_EQUAL: int = 1014
val SLASH_EQUAL: int = 1015
val PERCENT_EQUAL: int = 1016
val DOUBLE_EQUAL: int = 1017
val BANG_EQUAL: int = 1018
val GREATER_EQUAL: int = 1019
val LESS_EQUAL: int = 1020
val DOUBLE_GREATER: int = 1021
val DOUBLE_LESS: int = 1022
val DOUBLE_PIPE: int = 1023
val DOUBLE_AMPERSAND: int = 1024
val DOUBLE_STAR: int = 1025
val DOUBLE_PLUS: int = 1026
val DOUBLE_MINUS: int = 1027
val DOUBLE_DOT: int = 1028
val ARROW: int = 1029
val FAT_ARROW: int = 1030
val DOUBLE_COLON: int = 1031
val EQUAL: int = 1032
val GREATER: int = 1033
val LESS: int = 1034
val CARET: int = 1035
val BANG: int = 1036
val PLUS: int = 1037
val MINUS: int = 1038
val STAR: int = 1039
val SLASH: int = 1040
val PERCENT: int = 1041
val AT: int = 1042
val DOLLAR: int = 1043
val LEFT_PAREN: int = 1044
val RIGHT_PAREN: int = 1045
val LEFT_BRACKET: int = 1046
val RIGHT_BRACKET: int = 1047
val LEFT_BRACE: int = 1048
val RIGHT_BRACE: int = 1049
val SEMICOLON: int = 1050
val COMMA: int = 1051
val QUESTION: int = 1052
val COLON: int = 1053
val DOT: int = 1054
val BACKSLASH: int = 1055


val DOUBLE_LESS_EQUAL_PATTERN: pointer<char> = "<<="
val DOUBLE_GREATER_EQUAL_PATTERN: pointer<char> = ">>="
val BANG_CARET_EQUAL_PATTERN: pointer<char> = "!^="
val DOUBLE_STAR_EQUAL_PATTERN: pointer<char> = "**="
val QUESTION_ARROW_PATTERN: pointer<char> = "?->"
val BANG_DOUBLE_AMPERSAND_PATTERN: pointer<char> = "!&&"
val BANG_DOUBLE_PIPE_PATTERN: pointer<char> = "!||"
val NOT_ARROW_PATTERN: pointer<char> = "!->"
val DOUBLE_ARROW_PATTERN: pointer<char> = "<->"
val TRIPLE_EQUAL_PATTERN: pointer<char> = "==="
val PIPE_EQUAL_PATTERN: pointer<char> = "|="
val CARET_EQUAL_PATTERN: pointer<char> = "^="
val PLUS_EQUAL_PATTERN: pointer<char> = "+="
val MINUS_EQUAL_PATTERN: pointer<char> = "-="
val STAR_EQUAL_PATTERN: pointer<char> = "*="
val SLASH_EQUAL_PATTERN: pointer<char> = "/="
val PERCENT_EQUAL_PATTERN: pointer<char> = "%="
val DOUBLE_EQUAL_PATTERN: pointer<char> = "=="
val BANG_EQUAL_PATTERN: pointer<char> = "!="
val GREATER_EQUAL_PATTERN: pointer<char> = ">="
val LESS_EQUAL_PATTERN: pointer<char> = "<="
val DOUBLE_GREATER_PATTERN: pointer<char> = ">>"
val DOUBLE_LESS_PATTERN: pointer<char> = "<<"
val DOUBLE_PIPE_PATTERN: pointer<char> = "||"
val DOUBLE_AMPERSAND_PATTERN: pointer<char> = "&&"
val DOUBLE_STAR_PATTERN: pointer<char> = "**"
val DOUBLE_PLUS_PATTERN: pointer<char> = "++"
val DOUBLE_MINUS_PATTERN: pointer<char> = "--"
val DOUBLE_DOT_PATTERN: pointer<char> = ".."
val ARROW_PATTERN: pointer<char> = "->"
val FAT_ARROW_PATTERN: pointer<char> = "=>"
val DOUBLE_COLON_PATTERN: pointer<char> = "::"
val EQUAL_PATTERN: pointer<char> = "="
val GREATER_PATTERN: pointer<char> = ">"
val LESS_PATTERN: pointer<char> = "<"
val CARET_PATTERN: pointer<char> = "^"
val BANG_PATTERN: pointer<char> = "!"
val PLUS_PATTERN: pointer<char> = "+"
val MINUS_PATTERN: pointer<char> = "-"
val STAR_PATTERN: pointer<char> = "*"
val SLASH_PATTERN: pointer<char> = "/"
val PERCENT_PATTERN: pointer<char> = "%"
val AT_PATTERN: pointer<char> = "@"
val DOLLAR_PATTERN: pointer<char> = "$"
val LEFT_PAREN_PATTERN: pointer<char> = "("
val RIGHT_PAREN_PATTERN: pointer<char> = ")"
val LEFT_BRACKET_PATTERN: pointer<char> = "["
val RIGHT_BRACKET_PATTERN: pointer<char> = "]"
val LEFT_BRACE_PATTERN: pointer<char> = "{"
val RIGHT_BRACE_PATTERN: pointer<char> = "}"
val SEMICOLON_PATTERN: pointer<char> = ";"
val COMMA_PATTERN: pointer<char> = ","
val QUESTION_PATTERN: pointer<char> = "?"
val COLON_PATTERN: pointer<char> = ":"
val DOT_PATTERN: pointer<char> = "."
val BACKSLASH_PATTERN: pointer<char> = "\\"


var tokenizerIsInit: bool = false
val ruleLength: int = 89
val rulesSpace: blob[sizeof(pointer<TokenizeRule>) * 89]
val rulePtr: pointer<pointer<TokenizeRule>> = rulesSpace as pointer<pointer<TokenizeRule>>
val rule0: pointer<TokenizeRule> = new TokenizeRule(0, TokenizeFSM.DEFAULT, "\\0", eatEOF)
val rule1: pointer<TokenizeRule> = new TokenizeRule(1, TokenizeFSM.DEFAULT, "//", begainLineComment)
val rule2: pointer<TokenizeRule> = new TokenizeRule(2, TokenizeFSM.DEFAULT, "/\\*", begainBlockComment)
val rule3: pointer<TokenizeRule> = new TokenizeRule(3, TokenizeFSM.DEFAULT, "\\'", begainChar)
val rule4: pointer<TokenizeRule> = new TokenizeRule(4, TokenizeFSM.DEFAULT, "\"", begainString)
val rule5: pointer<TokenizeRule> = new TokenizeRule(5, TokenizeFSM.DEFAULT, "\\r?\\n", eatLineTerminator)
val rule6: pointer<TokenizeRule> = new TokenizeRule(6, TokenizeFSM.DEFAULT, "[\\ \\t\\r\\n\\f\\v]", skip)
val rule7: pointer<TokenizeRule> = new TokenizeRule(7, TokenizeFSM.DEFAULT, "[0-9]*\\.[0-9]+[eE][\\-\\+]?[0-9]+[lL]|[0-9]*\\.[0-9]+[lL]", eatLongDoubleLit)
val rule8: pointer<TokenizeRule> = new TokenizeRule(8, TokenizeFSM.DEFAULT, "[0-9]*\\.[0-9]+[eE][\\-\\+]?[0-9]+[fF]|[0-9]*\\.[0-9]+[fF]", eatFloatLit)
val rule9: pointer<TokenizeRule> = new TokenizeRule(9, TokenizeFSM.DEFAULT, "[0-9]*\\.[0-9]+[eE][\\-\\+]?[0-9]+|[0-9]*\\.[0-9]+", eatDoubleLit)
val rule10: pointer<TokenizeRule> = new TokenizeRule(10, TokenizeFSM.DEFAULT, "0[xX][0-9a-fA-F]+[lL]|[0-9]+[lL]", eatLongLit)
val rule11: pointer<TokenizeRule> = new TokenizeRule(11, TokenizeFSM.DEFAULT, "0[xX][0-9a-fA-F]+", eatIntLit)
val rule12: pointer<TokenizeRule> = new TokenizeRule(12, TokenizeFSM.DEFAULT, "[0-9]+[a-zA-Z_][a-zA-Z0-9_]*", invalidIdentError)
val rule13: pointer<TokenizeRule> = new TokenizeRule(13, TokenizeFSM.DEFAULT, "[0-9]+", eatIntLit)
val rule14: pointer<TokenizeRule> = new TokenizeRule(14, TokenizeFSM.DEFAULT, "<<=", eatDoubleLessEqual)
val rule15: pointer<TokenizeRule> = new TokenizeRule(15, TokenizeFSM.DEFAULT, ">>=", eatDoubleGreaterEqual)
val rule16: pointer<TokenizeRule> = new TokenizeRule(16, TokenizeFSM.DEFAULT, "!\\^=", eatBangCaretEqual)
val rule17: pointer<TokenizeRule> = new TokenizeRule(17, TokenizeFSM.DEFAULT, "\\*\\*=", eatDoubleStarEqual)
val rule18: pointer<TokenizeRule> = new TokenizeRule(18, TokenizeFSM.DEFAULT, "\\?->", eatQuestionArrow)
val rule19: pointer<TokenizeRule> = new TokenizeRule(19, TokenizeFSM.DEFAULT, "!&&", eatBangDoubleAmpersand)
val rule20: pointer<TokenizeRule> = new TokenizeRule(20, TokenizeFSM.DEFAULT, "!\\|\\|", eatBangDoublePipe)
val rule21: pointer<TokenizeRule> = new TokenizeRule(21, TokenizeFSM.DEFAULT, "!->", eatNotArrow)
val rule22: pointer<TokenizeRule> = new TokenizeRule(22, TokenizeFSM.DEFAULT, "<->", eatDoubleArrow)
val rule23: pointer<TokenizeRule> = new TokenizeRule(23, TokenizeFSM.DEFAULT, "===", eatTripleEqual)
val rule24: pointer<TokenizeRule> = new TokenizeRule(24, TokenizeFSM.DEFAULT, "\\|=", eatPipeEqual)
val rule25: pointer<TokenizeRule> = new TokenizeRule(25, TokenizeFSM.DEFAULT, "\\^=", eatCaretEqual)
val rule26: pointer<TokenizeRule> = new TokenizeRule(26, TokenizeFSM.DEFAULT, "\\+=", eatPlusEqual)
val rule27: pointer<TokenizeRule> = new TokenizeRule(27, TokenizeFSM.DEFAULT, "-=", eatMinusEqual)
val rule28: pointer<TokenizeRule> = new TokenizeRule(28, TokenizeFSM.DEFAULT, "\\*=", eatStarEqual)
val rule29: pointer<TokenizeRule> = new TokenizeRule(29, TokenizeFSM.DEFAULT, "/=", eatSlashEqual)
val rule30: pointer<TokenizeRule> = new TokenizeRule(30, TokenizeFSM.DEFAULT, "%=", eatPercentEqual)
val rule31: pointer<TokenizeRule> = new TokenizeRule(31, TokenizeFSM.DEFAULT, "==", eatDoubleEqual)
val rule32: pointer<TokenizeRule> = new TokenizeRule(32, TokenizeFSM.DEFAULT, "!=", eatBangEqual)
val rule33: pointer<TokenizeRule> = new TokenizeRule(33, TokenizeFSM.DEFAULT, ">=", eatGreaterEqual)
val rule34: pointer<TokenizeRule> = new TokenizeRule(34, TokenizeFSM.DEFAULT, "<=", eatLessEqual)
val rule35: pointer<TokenizeRule> = new TokenizeRule(35, TokenizeFSM.DEFAULT, ">>", eatDoubleGreater)
val rule36: pointer<TokenizeRule> = new TokenizeRule(36, TokenizeFSM.DEFAULT, "<<", eatDoubleLess)
val rule37: pointer<TokenizeRule> = new TokenizeRule(37, TokenizeFSM.DEFAULT, "\\|\\|", eatDoublePipe)
val rule38: pointer<TokenizeRule> = new TokenizeRule(38, TokenizeFSM.DEFAULT, "&&", eatDoubleAmpersand)
val rule39: pointer<TokenizeRule> = new TokenizeRule(39, TokenizeFSM.DEFAULT, "\\*\\*", eatDoubleStar)
val rule40: pointer<TokenizeRule> = new TokenizeRule(40, TokenizeFSM.DEFAULT, "\\+\\+", eatDoublePlus)
val rule41: pointer<TokenizeRule> = new TokenizeRule(41, TokenizeFSM.DEFAULT, "--", eatDoubleMinus)
val rule42: pointer<TokenizeRule> = new TokenizeRule(42, TokenizeFSM.DEFAULT, "\\.\\.", eatDoubleDot)
val rule43: pointer<TokenizeRule> = new TokenizeRule(43, TokenizeFSM.DEFAULT, "->", eatArrow)
val rule44: pointer<TokenizeRule> = new TokenizeRule(44, TokenizeFSM.DEFAULT, "=>", eatFatArrow)
val rule45: pointer<TokenizeRule> = new TokenizeRule(45, TokenizeFSM.DEFAULT, "::", eatDoubleColon)
val rule46: pointer<TokenizeRule> = new TokenizeRule(46, TokenizeFSM.DEFAULT, "=", eatEqual)
val rule47: pointer<TokenizeRule> = new TokenizeRule(47, TokenizeFSM.DEFAULT, ">", eatGreater)
val rule48: pointer<TokenizeRule> = new TokenizeRule(48, TokenizeFSM.DEFAULT, "<", eatLess)
val rule49: pointer<TokenizeRule> = new TokenizeRule(49, TokenizeFSM.DEFAULT, "\\^", eatCaret)
val rule50: pointer<TokenizeRule> = new TokenizeRule(50, TokenizeFSM.DEFAULT, "!", eatBang)
val rule51: pointer<TokenizeRule> = new TokenizeRule(51, TokenizeFSM.DEFAULT, "\\+", eatPlus)
val rule52: pointer<TokenizeRule> = new TokenizeRule(52, TokenizeFSM.DEFAULT, "-", eatMinus)
val rule53: pointer<TokenizeRule> = new TokenizeRule(53, TokenizeFSM.DEFAULT, "\\*", eatStar)
val rule54: pointer<TokenizeRule> = new TokenizeRule(54, TokenizeFSM.DEFAULT, "/", eatSlash)
val rule55: pointer<TokenizeRule> = new TokenizeRule(55, TokenizeFSM.DEFAULT, "%", eatPercent)
val rule56: pointer<TokenizeRule> = new TokenizeRule(56, TokenizeFSM.DEFAULT, "@", eatAt)
val rule57: pointer<TokenizeRule> = new TokenizeRule(57, TokenizeFSM.DEFAULT, "\\$", eatDollar)
val rule58: pointer<TokenizeRule> = new TokenizeRule(58, TokenizeFSM.DEFAULT, "\\(", eatLeftParen)
val rule59: pointer<TokenizeRule> = new TokenizeRule(59, TokenizeFSM.DEFAULT, "\\)", eatRightParen)
val rule60: pointer<TokenizeRule> = new TokenizeRule(60, TokenizeFSM.DEFAULT, "\\[", eatLeftBracket)
val rule61: pointer<TokenizeRule> = new TokenizeRule(61, TokenizeFSM.DEFAULT, "\\]", eatRightBracket)
val rule62: pointer<TokenizeRule> = new TokenizeRule(62, TokenizeFSM.DEFAULT, "\\{", eatLeftBrace)
val rule63: pointer<TokenizeRule> = new TokenizeRule(63, TokenizeFSM.DEFAULT, "\\}", eatRightBrace)
val rule64: pointer<TokenizeRule> = new TokenizeRule(64, TokenizeFSM.DEFAULT, ";", eatSemicolon)
val rule65: pointer<TokenizeRule> = new TokenizeRule(65, TokenizeFSM.DEFAULT, ",", eatComma)
val rule66: pointer<TokenizeRule> = new TokenizeRule(66, TokenizeFSM.DEFAULT, "\\?", eatQuestion)
val rule67: pointer<TokenizeRule> = new TokenizeRule(67, TokenizeFSM.DEFAULT, ":", eatColon)
val rule68: pointer<TokenizeRule> = new TokenizeRule(68, TokenizeFSM.DEFAULT, "\\.", eatDot)
val rule69: pointer<TokenizeRule> = new TokenizeRule(69, TokenizeFSM.DEFAULT, "\\\\", eatBackslash)
val rule70: pointer<TokenizeRule> = new TokenizeRule(70, TokenizeFSM.DEFAULT, "[a-zA-Z_][a-zA-Z0-9_]*", eatIdent)
val rule71: pointer<TokenizeRule> = new TokenizeRule(71, TokenizeFSM.DEFAULT, ".", defaultError)
val rule72: pointer<TokenizeRule> = new TokenizeRule(72, LINE_COMMENT_STATE, "\\0", eatEOF)
val rule73: pointer<TokenizeRule> = new TokenizeRule(73, LINE_COMMENT_STATE, "\\r?\\n", endLineComment)
val rule74: pointer<TokenizeRule> = new TokenizeRule(74, LINE_COMMENT_STATE, ".", skip)
val rule75: pointer<TokenizeRule> = new TokenizeRule(75, BLOCK_COMMENT_STATE, "\\0", unterminatedBlockCommentError)
val rule76: pointer<TokenizeRule> = new TokenizeRule(76, BLOCK_COMMENT_STATE, "\\*/", endBlockComment)
val rule77: pointer<TokenizeRule> = new TokenizeRule(77, BLOCK_COMMENT_STATE, "\\r?\\n", skipNewLine)
val rule78: pointer<TokenizeRule> = new TokenizeRule(78, BLOCK_COMMENT_STATE, ".", skip)
val rule79: pointer<TokenizeRule> = new TokenizeRule(79, CHAR_STATE, "\\0", unterminatedCharError)
val rule80: pointer<TokenizeRule> = new TokenizeRule(80, CHAR_STATE, "\\r?\\n", unterminatedCharError)
val rule81: pointer<TokenizeRule> = new TokenizeRule(81, CHAR_STATE, "\\\\.", eatChar)
val rule82: pointer<TokenizeRule> = new TokenizeRule(82, CHAR_STATE, "\\'", endChar)
val rule83: pointer<TokenizeRule> = new TokenizeRule(83, CHAR_STATE, ".", eatChar)
val rule84: pointer<TokenizeRule> = new TokenizeRule(84, STRING_STATE, "\\0", unterminatedStringError)
val rule85: pointer<TokenizeRule> = new TokenizeRule(85, STRING_STATE, "\\r?\\n", unterminatedStringError)
val rule86: pointer<TokenizeRule> = new TokenizeRule(86, STRING_STATE, "\\\\.", eatChar)
val rule87: pointer<TokenizeRule> = new TokenizeRule(87, STRING_STATE, "\"", endString)
val rule88: pointer<TokenizeRule> = new TokenizeRule(88, STRING_STATE, ".", eatChar)


private inline fun eatEOF(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    new Token(Token.EOF_KIND, input.pos.toTokenPosition(input.textLength), "")


private inline fun skip(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.updateCursor(input.textLength)
    return null
}


private inline fun skipNewLine(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    val cursor: pointer<LexPosition> = dest.getCursorPtr()
    cursor.line++
    cursor.column = 0
    cursor.offset += input.textLength
    return null
}


private inline fun errorToken(input: pointer<LexInput>, errorType: int, dest: pointer<TokenizeFSM>, errorInfo: pointer<char>) -> pointer<Token> =
    new Token(errorType, input.pos.toTokenPosition(input.textLength), input.text, errorInfo)


private fun defaultError(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    val tokenText: pointer<char> = System.allocMemory((String.strlen(input.text) + 16) * sizeof(char)) as pointer<char>
    String.strcpy(tokenText, INVALID_TOKEN_ERROR_MSG)
    String.strcat(tokenText, input.text)
    return errorToken(input, -TokenizeFSM.DEFAULT, dest, tokenText)
}


private fun invalidIdentError(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    val tokenText: pointer<char> = System.allocMemory((String.strlen(input.text) + 32) * sizeof(char)) as pointer<char>
    String.strcpy(tokenText, INVALID_IDENTITY_NAME_ERROR_MSG)
    String.strcat(tokenText, input.text)
    return errorToken(input, -TokenizeFSM.DEFAULT, dest, tokenText)
}


private inline fun changeState(input: pointer<LexInput>, state: int, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.setState(state)
    dest.updateCursor(input.textLength)
    return null
}


private fun begainLineComment(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    changeState(input, LINE_COMMENT_STATE, dest)


private fun begainBlockComment(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    changeState(input, BLOCK_COMMENT_STATE, dest)


private fun begainChar(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.accumulator.clear()
    return changeState(input, CHAR_STATE, dest)
}


private fun eatLineTerminator(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    skipNewLine(input, dest)
    return new Token(TK_LINE_TERMINATOR, input.pos.toTokenPosition(input.textLength), null)
}


private fun begainString(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.accumulator.clear()
    return changeState(input, STRING_STATE, dest)
}


private inline fun eatToken(input: pointer<LexInput>, numberType: int, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.updateCursor(input.textLength)
    return new Token(numberType, input.pos.toTokenPosition(input.textLength), input.text)
}


private fun eatIntLit(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, TK_INTEGER, dest)


private fun eatLongLit(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, TK_LONG, dest)


private fun eatFloatLit(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, TK_FLOAT, dest)


private fun eatDoubleLit(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, TK_DOUBLE, dest)


private fun eatLongDoubleLit(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, TK_LONG_DOUBLE, dest)


private inline fun getKeywordKind(text: pointer<char>) -> int
{
    var left: int = 0
    var right: int = keywordListLength - 1

    while left <= right:
    {
        val mid: int = (left + right) / 2
        val cmp: int = String.strcmp(text, getKeywordText(mid))

        if cmp == 0:
            return tokenDefStart + mid

        if cmp < 0:
            right = mid - 1
        else:
            left = mid + 1
    }

    return TK_IDENTITY
}


private fun eatIdent(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    val kind: int = getKeywordKind(input.text)
    return eatToken(input, kind, dest)
}


private fun endLineComment(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.setState(TokenizeFSM.DEFAULT)
    return eatLineTerminator(input, dest)
}


private fun unterminatedBlockCommentError(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    errorToken(input, -BLOCK_COMMENT_STATE, dest, UNTERMINATED_BLOCK_COMMENT_ERROR_MSG)


private fun endBlockComment(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.setState(TokenizeFSM.DEFAULT)
    dest.updateCursor(input.textLength)
    return new Token(TK_LINE_TERMINATOR, input.pos.toTokenPosition(input.textLength), null)
}


private fun unterminatedCharError(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    errorToken(input, -CHAR_STATE, dest, UNTERMINATED_CHAR_ERROR_MSG)


private fun endChar(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    val charText: pointer<char> = System.allocMemory((dest.accumulator.length + 1) * sizeof(char)) as pointer<char>
    dest.accumulator.toString(charText)
    val pos: pointer<TokenPosition> = new TokenPosition(
        input.pos.offset - dest.accumulator.length,
        input.pos.line,
        input.pos.column - dest.accumulator.length,
        dest.accumulator.length)
    changeState(input, TokenizeFSM.DEFAULT, dest)
    return new Token(TK_CHAR, pos, charText)
}


private fun eatChar(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.accumulator.append(input.text)
    dest.updateCursor(input.textLength)
    return null
}


private fun unterminatedStringError(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    errorToken(input, -STRING_STATE, dest, UNTERMINATED_STRING_ERROR_MSG)


private fun endString(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    val charText: pointer<char> = System.allocMemory((dest.accumulator.length + 1) * sizeof(char)) as pointer<char>
    dest.accumulator.toString(charText)
    val pos: pointer<TokenPosition> = new TokenPosition(
        input.pos.offset - dest.accumulator.length,
        input.pos.line,
        input.pos.column - dest.accumulator.length,
        dest.accumulator.length)
    changeState(input, TokenizeFSM.DEFAULT, dest)
    return new Token(TK_STRING, pos, charText)
}


fun fullTokenize(code: pointer<char>) -> pointer<TokenList>
{
    val tokens: pointer<TokenList> = tokenize(code)
    return TokenNormalizer.normalize(tokens)
}




private fun eatDoubleLessEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_LESS_EQUAL, dest)

private fun eatDoubleGreaterEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_GREATER_EQUAL, dest)

private fun eatBangCaretEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, BANG_CARET_EQUAL, dest)

private fun eatDoubleStarEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_STAR_EQUAL, dest)

private fun eatQuestionArrow(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, QUESTION_ARROW, dest)

private fun eatBangDoubleAmpersand(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, BANG_DOUBLE_AMPERSAND, dest)

private fun eatBangDoublePipe(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, BANG_DOUBLE_PIPE, dest)

private fun eatNotArrow(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, NOT_ARROW, dest)

private fun eatDoubleArrow(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_ARROW, dest)

private fun eatTripleEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, TRIPLE_EQUAL, dest)

private fun eatPipeEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, PIPE_EQUAL, dest)

private fun eatCaretEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, CARET_EQUAL, dest)

private fun eatPlusEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, PLUS_EQUAL, dest)

private fun eatMinusEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, MINUS_EQUAL, dest)

private fun eatStarEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, STAR_EQUAL, dest)

private fun eatSlashEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, SLASH_EQUAL, dest)

private fun eatPercentEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, PERCENT_EQUAL, dest)

private fun eatDoubleEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_EQUAL, dest)

private fun eatBangEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, BANG_EQUAL, dest)

private fun eatGreaterEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, GREATER_EQUAL, dest)

private fun eatLessEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, LESS_EQUAL, dest)

private fun eatDoubleGreater(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_GREATER, dest)

private fun eatDoubleLess(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_LESS, dest)

private fun eatDoublePipe(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_PIPE, dest)

private fun eatDoubleAmpersand(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_AMPERSAND, dest)

private fun eatDoubleStar(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_STAR, dest)

private fun eatDoublePlus(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_PLUS, dest)

private fun eatDoubleMinus(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_MINUS, dest)

private fun eatDoubleDot(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_DOT, dest)

private fun eatArrow(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, ARROW, dest)

private fun eatFatArrow(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, FAT_ARROW, dest)

private fun eatDoubleColon(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOUBLE_COLON, dest)

private fun eatEqual(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, EQUAL, dest)

private fun eatGreater(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, GREATER, dest)

private fun eatLess(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, LESS, dest)

private fun eatCaret(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, CARET, dest)

private fun eatBang(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, BANG, dest)

private fun eatPlus(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, PLUS, dest)

private fun eatMinus(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, MINUS, dest)

private fun eatStar(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, STAR, dest)

private fun eatSlash(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, SLASH, dest)

private fun eatPercent(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, PERCENT, dest)

private fun eatAt(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, AT, dest)

private fun eatDollar(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOLLAR, dest)

private fun eatLeftParen(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, LEFT_PAREN, dest)

private fun eatRightParen(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, RIGHT_PAREN, dest)

private fun eatLeftBracket(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, LEFT_BRACKET, dest)

private fun eatRightBracket(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, RIGHT_BRACKET, dest)

private fun eatLeftBrace(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, LEFT_BRACE, dest)

private fun eatRightBrace(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, RIGHT_BRACE, dest)

private fun eatSemicolon(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, SEMICOLON, dest)

private fun eatComma(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, COMMA, dest)

private fun eatQuestion(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, QUESTION, dest)

private fun eatColon(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, COLON, dest)

private fun eatDot(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, DOT, dest)

private fun eatBackslash(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    eatToken(input, BACKSLASH, dest)


private fun tokenizerInit()
{
    rulePtr[0] = rule0
    rulePtr[1] = rule1
    rulePtr[2] = rule2
    rulePtr[3] = rule3
    rulePtr[4] = rule4
    rulePtr[5] = rule5
    rulePtr[6] = rule6
    rulePtr[7] = rule7
    rulePtr[8] = rule8
    rulePtr[9] = rule9
    rulePtr[10] = rule10
    rulePtr[11] = rule11
    rulePtr[12] = rule12
    rulePtr[13] = rule13
    rulePtr[14] = rule14
    rulePtr[15] = rule15
    rulePtr[16] = rule16
    rulePtr[17] = rule17
    rulePtr[18] = rule18
    rulePtr[19] = rule19
    rulePtr[20] = rule20
    rulePtr[21] = rule21
    rulePtr[22] = rule22
    rulePtr[23] = rule23
    rulePtr[24] = rule24
    rulePtr[25] = rule25
    rulePtr[26] = rule26
    rulePtr[27] = rule27
    rulePtr[28] = rule28
    rulePtr[29] = rule29
    rulePtr[30] = rule30
    rulePtr[31] = rule31
    rulePtr[32] = rule32
    rulePtr[33] = rule33
    rulePtr[34] = rule34
    rulePtr[35] = rule35
    rulePtr[36] = rule36
    rulePtr[37] = rule37
    rulePtr[38] = rule38
    rulePtr[39] = rule39
    rulePtr[40] = rule40
    rulePtr[41] = rule41
    rulePtr[42] = rule42
    rulePtr[43] = rule43
    rulePtr[44] = rule44
    rulePtr[45] = rule45
    rulePtr[46] = rule46
    rulePtr[47] = rule47
    rulePtr[48] = rule48
    rulePtr[49] = rule49
    rulePtr[50] = rule50
    rulePtr[51] = rule51
    rulePtr[52] = rule52
    rulePtr[53] = rule53
    rulePtr[54] = rule54
    rulePtr[55] = rule55
    rulePtr[56] = rule56
    rulePtr[57] = rule57
    rulePtr[58] = rule58
    rulePtr[59] = rule59
    rulePtr[60] = rule60
    rulePtr[61] = rule61
    rulePtr[62] = rule62
    rulePtr[63] = rule63
    rulePtr[64] = rule64
    rulePtr[65] = rule65
    rulePtr[66] = rule66
    rulePtr[67] = rule67
    rulePtr[68] = rule68
    rulePtr[69] = rule69
    rulePtr[70] = rule70
    rulePtr[71] = rule71
    rulePtr[72] = rule72
    rulePtr[73] = rule73
    rulePtr[74] = rule74
    rulePtr[75] = rule75
    rulePtr[76] = rule76
    rulePtr[77] = rule77
    rulePtr[78] = rule78
    rulePtr[79] = rule79
    rulePtr[80] = rule80
    rulePtr[81] = rule81
    rulePtr[82] = rule82
    rulePtr[83] = rule83
    rulePtr[84] = rule84
    rulePtr[85] = rule85
    rulePtr[86] = rule86
    rulePtr[87] = rule87
    rulePtr[88] = rule88
    keywordListInit()
    tokenizerIsInit = true
}


fun tokenize(code: pointer<char>) -> pointer<TokenList>
{
    if !tokenizerIsInit:
        tokenizerInit()

    val tokenList: pointer<TokenList> = new TokenList()
    val lexState: pointer<TokenizeFSM> = new TokenizeFSM(code)

    while true:
    {
        val token: pointer<Token> = lexState.apply(rulePtr, ruleLength)

        if token == null:
            continue

        tokenList.push(token)

        if token.kind < 0 || token.isEOF():
            break
    }

    return tokenList
}


