@file.class("Tokenizer")
package xlang.compiler.lexer


import xlang.System
import xlang.lexer.Lex
import xlang.lexer.LexPosition
import xlang.lexer.LexState
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.lexer.TokenizerHelper
import xlang.util.string.String
import xlang.util.string.StringBuilder


val INVALID_TOKEN_ERROR_MSG: pointer<char> = "invalid token: "
val UNTERMINATED_BLOCK_COMMENT_ERROR_MSG: pointer<char> = "unterminated block comment"
val UNTERMINATED_CHAR_ERROR_MSG: pointer<char> = "unterminated char comment"
val UNTERMINATED_STRING_ERROR_MSG: pointer<char> = "unterminated string comment"
val INVALID_IDENTITY_NAME_ERROR_MSG: pointer<char> = "invalid identity name: "
val LINE_COMMENT_STATE: int = 10
val BLOCK_COMMENT_STATE: int = 11
val CHAR_STATE: int = 12
val STRING_STATE: int = 13
val TK_TOKEN_PASS: int = 1
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
val PIPE_EQUAL: int = 1009
val CARET_EQUAL: int = 1010
val PLUS_EQUAL: int = 1011
val MINUS_EQUAL: int = 1012
val STAR_EQUAL: int = 1013
val SLASH_EQUAL: int = 1014
val PERCENT_EQUAL: int = 1015
val DOUBLE_EQUAL: int = 1016
val BANG_EQUAL: int = 1017
val GREATER_EQUAL: int = 1018
val LESS_EQUAL: int = 1019
val DOUBLE_GREATER: int = 1020
val DOUBLE_LESS: int = 1021
val DOUBLE_PIPE: int = 1022
val DOUBLE_AMPERSAND: int = 1023
val DOUBLE_STAR: int = 1024
val DOUBLE_PLUS: int = 1025
val DOUBLE_MINUS: int = 1026
val DOUBLE_DOT: int = 1027
val ARROW: int = 1028
val FAT_ARROW: int = 1029
val DOUBLE_COLON: int = 1030
val EQUAL: int = 1031
val GREATER: int = 1032
val LESS: int = 1033
val CARET: int = 1034
val BANG: int = 1035
val PLUS: int = 1036
val MINUS: int = 1037
val STAR: int = 1038
val SLASH: int = 1039
val PERCENT: int = 1040
val AT: int = 1041
val DOLLAR: int = 1042
val LEFT_PAREN: int = 1043
val RIGHT_PAREN: int = 1044
val LEFT_BRACKET: int = 1045
val RIGHT_BRACKET: int = 1046
val LEFT_BRACE: int = 1047
val RIGHT_BRACE: int = 1048
val SEMICOLON: int = 1049
val COMMA: int = 1050
val QUESTION: int = 1051
val COLON: int = 1052
val DOT: int = 1053
val BACKSLASH: int = 1054


val DOUBLE_LESS_EQUAL_PATTERN: pointer<char> = "<<="
val DOUBLE_GREATER_EQUAL_PATTERN: pointer<char> = ">>="
val BANG_CARET_EQUAL_PATTERN: pointer<char> = "!^="
val DOUBLE_STAR_EQUAL_PATTERN: pointer<char> = "**="
val QUESTION_ARROW_PATTERN: pointer<char> = "?->"
val BANG_DOUBLE_AMPERSAND_PATTERN: pointer<char> = "!&&"
val BANG_DOUBLE_PIPE_PATTERN: pointer<char> = "!||"
val NOT_ARROW_PATTERN: pointer<char> = "!->"
val DOUBLE_ARROW_PATTERN: pointer<char> = "<->"
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
val ruleLength: int = 88
val rulesSpace: blob[sizeof(pointer<Rule>) * 88]
val rulePtr: pointer<pointer<Rule>> = rulesSpace as pointer<pointer<Rule>>
val rule0: Rule = Rule(0, LexState.DEFAULT, "\\0", eatEOF)
val rule1: Rule = Rule(1, LexState.DEFAULT, "//", begainLineComment)
val rule2: Rule = Rule(2, LexState.DEFAULT, "/\\*", begainBlockComment)
val rule3: Rule = Rule(3, LexState.DEFAULT, "\\'", begainChar)
val rule4: Rule = Rule(4, LexState.DEFAULT, "\"", begainString)
val rule5: Rule = Rule(5, LexState.DEFAULT, "\\r?\\n", tokenPass)
val rule6: Rule = Rule(6, LexState.DEFAULT, "[\\ \\t\\r\\n\\f\\v]", skip)
val rule7: Rule = Rule(7, LexState.DEFAULT, "[0-9]*\\.[0-9]+[eE][\\-\\+]?[0-9]+[lL]|[0-9]*\\.[0-9]+[lL]", eatLongDoubleLit)
val rule8: Rule = Rule(8, LexState.DEFAULT, "[0-9]*\\.[0-9]+[eE][\\-\\+]?[0-9]+[fF]|[0-9]*\\.[0-9]+[fF]", eatFloatLit)
val rule9: Rule = Rule(9, LexState.DEFAULT, "[0-9]*\\.[0-9]+[eE][\\-\\+]?[0-9]+|[0-9]*\\.[0-9]+", eatDoubleLit)
val rule10: Rule = Rule(10, LexState.DEFAULT, "0[xX][0-9a-fA-F]+[lL]|[0-9]+[lL]", eatLongLit)
val rule11: Rule = Rule(11, LexState.DEFAULT, "0[xX][0-9a-fA-F]+", eatIntLit)
val rule12: Rule = Rule(12, LexState.DEFAULT, "[0-9]+[a-zA-Z_][a-zA-Z0-9_]*", invalidIdentError)
val rule13: Rule = Rule(13, LexState.DEFAULT, "[0-9]+", eatIntLit)
val rule14: Rule = Rule(14, LexState.DEFAULT, "<<=", eatDoubleLessEqual)
val rule15: Rule = Rule(15, LexState.DEFAULT, ">>=", eatDoubleGreaterEqual)
val rule16: Rule = Rule(16, LexState.DEFAULT, "!\\^=", eatBangCaretEqual)
val rule17: Rule = Rule(17, LexState.DEFAULT, "\\*\\*=", eatDoubleStarEqual)
val rule18: Rule = Rule(18, LexState.DEFAULT, "\\?->", eatQuestionArrow)
val rule19: Rule = Rule(19, LexState.DEFAULT, "!&&", eatBangDoubleAmpersand)
val rule20: Rule = Rule(20, LexState.DEFAULT, "!\\|\\|", eatBangDoublePipe)
val rule21: Rule = Rule(21, LexState.DEFAULT, "!->", eatNotArrow)
val rule22: Rule = Rule(22, LexState.DEFAULT, "<->", eatDoubleArrow)
val rule23: Rule = Rule(23, LexState.DEFAULT, "\\|=", eatPipeEqual)
val rule24: Rule = Rule(24, LexState.DEFAULT, "\\^=", eatCaretEqual)
val rule25: Rule = Rule(25, LexState.DEFAULT, "\\+=", eatPlusEqual)
val rule26: Rule = Rule(26, LexState.DEFAULT, "-=", eatMinusEqual)
val rule27: Rule = Rule(27, LexState.DEFAULT, "\\*=", eatStarEqual)
val rule28: Rule = Rule(28, LexState.DEFAULT, "/=", eatSlashEqual)
val rule29: Rule = Rule(29, LexState.DEFAULT, "%=", eatPercentEqual)
val rule30: Rule = Rule(30, LexState.DEFAULT, "==", eatDoubleEqual)
val rule31: Rule = Rule(31, LexState.DEFAULT, "!=", eatBangEqual)
val rule32: Rule = Rule(32, LexState.DEFAULT, ">=", eatGreaterEqual)
val rule33: Rule = Rule(33, LexState.DEFAULT, "<=", eatLessEqual)
val rule34: Rule = Rule(34, LexState.DEFAULT, ">>", eatDoubleGreater)
val rule35: Rule = Rule(35, LexState.DEFAULT, "<<", eatDoubleLess)
val rule36: Rule = Rule(36, LexState.DEFAULT, "\\|\\|", eatDoublePipe)
val rule37: Rule = Rule(37, LexState.DEFAULT, "&&", eatDoubleAmpersand)
val rule38: Rule = Rule(38, LexState.DEFAULT, "\\*\\*", eatDoubleStar)
val rule39: Rule = Rule(39, LexState.DEFAULT, "\\+\\+", eatDoublePlus)
val rule40: Rule = Rule(40, LexState.DEFAULT, "--", eatDoubleMinus)
val rule41: Rule = Rule(41, LexState.DEFAULT, "\\.\\.", eatDoubleDot)
val rule42: Rule = Rule(42, LexState.DEFAULT, "->", eatArrow)
val rule43: Rule = Rule(43, LexState.DEFAULT, "=>", eatFatArrow)
val rule44: Rule = Rule(44, LexState.DEFAULT, "::", eatDoubleColon)
val rule45: Rule = Rule(45, LexState.DEFAULT, "=", eatEqual)
val rule46: Rule = Rule(46, LexState.DEFAULT, ">", eatGreater)
val rule47: Rule = Rule(47, LexState.DEFAULT, "<", eatLess)
val rule48: Rule = Rule(48, LexState.DEFAULT, "\\^", eatCaret)
val rule49: Rule = Rule(49, LexState.DEFAULT, "!", eatBang)
val rule50: Rule = Rule(50, LexState.DEFAULT, "\\+", eatPlus)
val rule51: Rule = Rule(51, LexState.DEFAULT, "-", eatMinus)
val rule52: Rule = Rule(52, LexState.DEFAULT, "\\*", eatStar)
val rule53: Rule = Rule(53, LexState.DEFAULT, "/", eatSlash)
val rule54: Rule = Rule(54, LexState.DEFAULT, "%", eatPercent)
val rule55: Rule = Rule(55, LexState.DEFAULT, "@", eatAt)
val rule56: Rule = Rule(56, LexState.DEFAULT, "\\$", eatDollar)
val rule57: Rule = Rule(57, LexState.DEFAULT, "\\(", eatLeftParen)
val rule58: Rule = Rule(58, LexState.DEFAULT, "\\)", eatRightParen)
val rule59: Rule = Rule(59, LexState.DEFAULT, "\\[", eatLeftBracket)
val rule60: Rule = Rule(60, LexState.DEFAULT, "\\]", eatRightBracket)
val rule61: Rule = Rule(61, LexState.DEFAULT, "\\{", eatLeftBrace)
val rule62: Rule = Rule(62, LexState.DEFAULT, "\\}", eatRightBrace)
val rule63: Rule = Rule(63, LexState.DEFAULT, ";", eatSemicolon)
val rule64: Rule = Rule(64, LexState.DEFAULT, ",", eatComma)
val rule65: Rule = Rule(65, LexState.DEFAULT, "\\?", eatQuestion)
val rule66: Rule = Rule(66, LexState.DEFAULT, ":", eatColon)
val rule67: Rule = Rule(67, LexState.DEFAULT, "\\.", eatDot)
val rule68: Rule = Rule(68, LexState.DEFAULT, "\\\\", eatBackslash)
val rule69: Rule = Rule(69, LexState.DEFAULT, "[a-zA-Z_][a-zA-Z0-9_]*", eatIdent)
val rule70: Rule = Rule(70, LexState.DEFAULT, ".", defaultError)
val rule71: Rule = Rule(71, LINE_COMMENT_STATE, "\\0", eatEOF)
val rule72: Rule = Rule(72, LINE_COMMENT_STATE, "\\r?\\n", endLineComment)
val rule73: Rule = Rule(73, LINE_COMMENT_STATE, ".", skip)
val rule74: Rule = Rule(74, BLOCK_COMMENT_STATE, "\\0", unterminatedBlockCommentError)
val rule75: Rule = Rule(75, BLOCK_COMMENT_STATE, "\\*/", endBlockComment)
val rule76: Rule = Rule(76, BLOCK_COMMENT_STATE, "\\r?\\n", skipNewLine)
val rule77: Rule = Rule(77, BLOCK_COMMENT_STATE, ".", skip)
val rule78: Rule = Rule(78, CHAR_STATE, "\\0", unterminatedCharError)
val rule79: Rule = Rule(79, CHAR_STATE, "\\r?\\n", unterminatedCharError)
val rule80: Rule = Rule(80, CHAR_STATE, "\\\\.", eatChar)
val rule81: Rule = Rule(81, CHAR_STATE, "\\'", endChar)
val rule82: Rule = Rule(82, CHAR_STATE, ".", eatChar)
val rule83: Rule = Rule(83, STRING_STATE, "\\0", unterminatedStringError)
val rule84: Rule = Rule(84, STRING_STATE, "\\r?\\n", unterminatedStringError)
val rule85: Rule = Rule(85, STRING_STATE, "\\\\.", eatChar)
val rule86: Rule = Rule(86, STRING_STATE, "\"", endString)
val rule87: Rule = Rule(87, STRING_STATE, ".", eatChar)


private inline fun eatEOF(input: LexInput, dest: LexState) -> Token =
    new Token(Token.EOF_KIND, input.pos.toTokenPosition(input.textLength), "")


private inline fun skip(input: LexInput, dest: LexState) -> Token
{
    dest.updateCursor(input.textLength)
    return null
}


private inline fun skipNewLine(input: LexInput, dest: LexState) -> Token
{
    val cursor: LexPosition = dest.getCursorPtr()
    cursor.line++
    cursor.column = 0
    cursor.offset += input.textLength
    return null
}


private inline fun errorToken(input: LexInput, errorType: int, dest: LexState, errorInfo: pointer<char>) -> Token =
    new Token(errorType, input.pos.toTokenPosition(input.textLength), input.text, errorInfo)


private fun defaultError(input: LexInput, dest: LexState) -> Token
{
    val tokenText: pointer<char> = System.allocMemory((String.strlen(input.text) + 16) * sizeof(char)) as pointer<char>
    String.strcpy(tokenText, INVALID_TOKEN_ERROR_MSG)
    String.strcat(tokenText, input.text)
    return errorToken(input, -LexState.DEFAULT, dest, tokenText)
}


private fun invalidIdentError(input: LexInput, dest: LexState) -> Token
{
    val tokenText: pointer<char> = System.allocMemory((String.strlen(input.text) + 32) * sizeof(char)) as pointer<char>
    String.strcpy(tokenText, INVALID_IDENTITY_NAME_ERROR_MSG)
    String.strcat(tokenText, input.text)
    return errorToken(input, -LexState.DEFAULT, dest, tokenText)
}


private inline fun changeState(input: LexInput, state: int, dest: LexState) -> Token
{
    dest.setState(state)
    dest.updateCursor(input.textLength)
    return null
}


private fun begainLineComment(input: LexInput, dest: LexState) -> Token =
    changeState(input, LINE_COMMENT_STATE, dest)


private fun begainBlockComment(input: LexInput, dest: LexState) -> Token =
    changeState(input, BLOCK_COMMENT_STATE, dest)


private fun begainChar(input: LexInput, dest: LexState) -> Token
{
    dest.accumulator.clear()
    return changeState(input, CHAR_STATE, dest)
}


private fun tokenPass(input: LexInput, dest: LexState) -> Token
{
    skipNewLine(input, dest)
    return new Token(TK_TOKEN_PASS, input.pos.toTokenPosition(input.textLength), null)
}


private fun begainString(input: LexInput, dest: LexState) -> Token
{
    dest.accumulator.clear()
    return changeState(input, STRING_STATE, dest)
}


private inline fun eatToken(input: LexInput, numberType: int, dest: LexState) -> Token
{
    dest.updateCursor(input.textLength)
    return new Token(numberType, input.pos.toTokenPosition(input.textLength), input.text)
}


private fun eatIntLit(input: LexInput, dest: LexState) -> Token =
    eatToken(input, TK_INTEGER, dest)


private fun eatLongLit(input: LexInput, dest: LexState) -> Token =
    eatToken(input, TK_LONG, dest)


private fun eatFloatLit(input: LexInput, dest: LexState) -> Token =
    eatToken(input, TK_FLOAT, dest)


private fun eatDoubleLit(input: LexInput, dest: LexState) -> Token =
    eatToken(input, TK_DOUBLE, dest)


private fun eatLongDoubleLit(input: LexInput, dest: LexState) -> Token =
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


private fun eatIdent(input: LexInput, dest: LexState) -> Token
{
    val kind: int = getKeywordKind(input.text)
    return eatToken(input, kind, dest)
}


private fun endLineComment(input: LexInput, dest: LexState) -> Token
{
    dest.setState(LexState.DEFAULT)
    val cursor: LexPosition = dest.getCursorPtr()
    cursor.line++
    cursor.column = 0
    cursor.offset += input.textLength
    return null
}


private fun unterminatedBlockCommentError(input: LexInput, dest: LexState) -> Token =
    errorToken(input, -BLOCK_COMMENT_STATE, dest, UNTERMINATED_BLOCK_COMMENT_ERROR_MSG)


private fun endBlockComment(input: LexInput, dest: LexState) -> Token =
    changeState(input, LexState.DEFAULT, dest)


private fun unterminatedCharError(input: LexInput, dest: LexState) -> Token =
    errorToken(input, -CHAR_STATE, dest, UNTERMINATED_CHAR_ERROR_MSG)


private fun endChar(input: LexInput, dest: LexState) -> Token
{
    val charText: pointer<char> = System.allocMemory((dest.accumulator.length + 1) * sizeof(char)) as pointer<char>
    dest.accumulator.get(charText)
    val pos: TokenPosition = new TokenPosition(
        input.pos.offset - dest.accumulator.length,
        input.pos.line,
        input.pos.column - dest.accumulator.length,
        dest.accumulator.length)
    changeState(input, LexState.DEFAULT, dest)
    return new Token(TK_CHAR, pos, charText)
}


private fun eatChar(input: LexInput, dest: LexState) -> Token
{
    dest.accumulator.append(input.text)
    dest.updateCursor(input.textLength)
    return null
}


private fun unterminatedStringError(input: LexInput, dest: LexState) -> Token =
    errorToken(input, -STRING_STATE, dest, UNTERMINATED_STRING_ERROR_MSG)


private fun endString(input: LexInput, dest: LexState) -> Token
{
    val charText: pointer<char> = System.allocMemory((dest.accumulator.length + 1) * sizeof(char)) as pointer<char>
    dest.accumulator.get(charText)
    val pos: TokenPosition = new TokenPosition(
        input.pos.offset - dest.accumulator.length,
        input.pos.line,
        input.pos.column - dest.accumulator.length,
        dest.accumulator.length)
    changeState(input, LexState.DEFAULT, dest)
    return new Token(TK_STRING, pos, charText)
}




private fun eatDoubleLessEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_LESS_EQUAL, dest)

private fun eatDoubleGreaterEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_GREATER_EQUAL, dest)

private fun eatBangCaretEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, BANG_CARET_EQUAL, dest)

private fun eatDoubleStarEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_STAR_EQUAL, dest)

private fun eatQuestionArrow(input: LexInput, dest: LexState) -> Token =
    eatToken(input, QUESTION_ARROW, dest)

private fun eatBangDoubleAmpersand(input: LexInput, dest: LexState) -> Token =
    eatToken(input, BANG_DOUBLE_AMPERSAND, dest)

private fun eatBangDoublePipe(input: LexInput, dest: LexState) -> Token =
    eatToken(input, BANG_DOUBLE_PIPE, dest)

private fun eatNotArrow(input: LexInput, dest: LexState) -> Token =
    eatToken(input, NOT_ARROW, dest)

private fun eatDoubleArrow(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_ARROW, dest)

private fun eatPipeEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, PIPE_EQUAL, dest)

private fun eatCaretEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, CARET_EQUAL, dest)

private fun eatPlusEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, PLUS_EQUAL, dest)

private fun eatMinusEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, MINUS_EQUAL, dest)

private fun eatStarEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, STAR_EQUAL, dest)

private fun eatSlashEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, SLASH_EQUAL, dest)

private fun eatPercentEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, PERCENT_EQUAL, dest)

private fun eatDoubleEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_EQUAL, dest)

private fun eatBangEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, BANG_EQUAL, dest)

private fun eatGreaterEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, GREATER_EQUAL, dest)

private fun eatLessEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, LESS_EQUAL, dest)

private fun eatDoubleGreater(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_GREATER, dest)

private fun eatDoubleLess(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_LESS, dest)

private fun eatDoublePipe(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_PIPE, dest)

private fun eatDoubleAmpersand(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_AMPERSAND, dest)

private fun eatDoubleStar(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_STAR, dest)

private fun eatDoublePlus(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_PLUS, dest)

private fun eatDoubleMinus(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_MINUS, dest)

private fun eatDoubleDot(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_DOT, dest)

private fun eatArrow(input: LexInput, dest: LexState) -> Token =
    eatToken(input, ARROW, dest)

private fun eatFatArrow(input: LexInput, dest: LexState) -> Token =
    eatToken(input, FAT_ARROW, dest)

private fun eatDoubleColon(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOUBLE_COLON, dest)

private fun eatEqual(input: LexInput, dest: LexState) -> Token =
    eatToken(input, EQUAL, dest)

private fun eatGreater(input: LexInput, dest: LexState) -> Token =
    eatToken(input, GREATER, dest)

private fun eatLess(input: LexInput, dest: LexState) -> Token =
    eatToken(input, LESS, dest)

private fun eatCaret(input: LexInput, dest: LexState) -> Token =
    eatToken(input, CARET, dest)

private fun eatBang(input: LexInput, dest: LexState) -> Token =
    eatToken(input, BANG, dest)

private fun eatPlus(input: LexInput, dest: LexState) -> Token =
    eatToken(input, PLUS, dest)

private fun eatMinus(input: LexInput, dest: LexState) -> Token =
    eatToken(input, MINUS, dest)

private fun eatStar(input: LexInput, dest: LexState) -> Token =
    eatToken(input, STAR, dest)

private fun eatSlash(input: LexInput, dest: LexState) -> Token =
    eatToken(input, SLASH, dest)

private fun eatPercent(input: LexInput, dest: LexState) -> Token =
    eatToken(input, PERCENT, dest)

private fun eatAt(input: LexInput, dest: LexState) -> Token =
    eatToken(input, AT, dest)

private fun eatDollar(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOLLAR, dest)

private fun eatLeftParen(input: LexInput, dest: LexState) -> Token =
    eatToken(input, LEFT_PAREN, dest)

private fun eatRightParen(input: LexInput, dest: LexState) -> Token =
    eatToken(input, RIGHT_PAREN, dest)

private fun eatLeftBracket(input: LexInput, dest: LexState) -> Token =
    eatToken(input, LEFT_BRACKET, dest)

private fun eatRightBracket(input: LexInput, dest: LexState) -> Token =
    eatToken(input, RIGHT_BRACKET, dest)

private fun eatLeftBrace(input: LexInput, dest: LexState) -> Token =
    eatToken(input, LEFT_BRACE, dest)

private fun eatRightBrace(input: LexInput, dest: LexState) -> Token =
    eatToken(input, RIGHT_BRACE, dest)

private fun eatSemicolon(input: LexInput, dest: LexState) -> Token =
    eatToken(input, SEMICOLON, dest)

private fun eatComma(input: LexInput, dest: LexState) -> Token =
    eatToken(input, COMMA, dest)

private fun eatQuestion(input: LexInput, dest: LexState) -> Token =
    eatToken(input, QUESTION, dest)

private fun eatColon(input: LexInput, dest: LexState) -> Token =
    eatToken(input, COLON, dest)

private fun eatDot(input: LexInput, dest: LexState) -> Token =
    eatToken(input, DOT, dest)

private fun eatBackslash(input: LexInput, dest: LexState) -> Token =
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
    keywordListInit()
    tokenizerIsInit = true
}


fun tokenize(code: pointer<char>) -> TokenList
{
    if !tokenizerIsInit:
        tokenizerInit()

    return TokenizerHelper.tokenize(code, rulePtr, ruleLength)
}


