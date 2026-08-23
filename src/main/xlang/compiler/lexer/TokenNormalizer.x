@file.class("TokenNormalizer")
package xlang.compiler.lexer


import xlang.System
import xlang.lexer.NormalizeFSM
import xlang.lexer.NormalizeRule
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.ArrayList
import xlang.util.HashSet


val banAfter: pointer<HashSet> = initBanAfter()
val banBefore: pointer<HashSet> = initBanBefore()


var normalizerIsInit: bool = false
val ruleLength: int = 8
val rulesSpace: blob[sizeof(pointer<NormalizeRule>) * 8]
val rulePtr: pointer<pointer<NormalizeRule>> = rulesSpace as pointer<pointer<NormalizeRule>>


private fun tokenKindCmp(left: pointer<*>, right: pointer<*>) -> int
{
    val lhs: int = (left as pointer<int>).deref
    val rhs: int = (right as pointer<int>).deref

    if lhs == rhs:
        return 0

    return (if lhs < rhs: -1 else: 1)
}

private inline fun addBanKind(set: pointer<HashSet>, kind: int) -> pointer<HashSet>
{
    var kindSpace: int = kind
    val kindPtr: pointer<int> = kindSpace.ref

    return set.add(kindPtr)
}

private fun addCommonBanKinds(set: pointer<HashSet>)
{
    addBanKind(set, Tokenizer.DOUBLE_LESS_EQUAL)
    addBanKind(set, Tokenizer.DOUBLE_GREATER_EQUAL)
    addBanKind(set, Tokenizer.TRIPLE_GREATER_EQUAL)
    addBanKind(set, Tokenizer.BANG_CARET_EQUAL)
    addBanKind(set, Tokenizer.DOUBLE_STAR_EQUAL)
    addBanKind(set, Tokenizer.BANG_DOUBLE_AMPERSAND)
    addBanKind(set, Tokenizer.BANG_DOUBLE_PIPE)
    addBanKind(set, Tokenizer.NOT_ARROW)
    addBanKind(set, Tokenizer.DOUBLE_ARROW)
    addBanKind(set, Tokenizer.TRIPLE_EQUAL)
    addBanKind(set, Tokenizer.PIPE_EQUAL)
    addBanKind(set, Tokenizer.CARET_EQUAL)
    addBanKind(set, Tokenizer.PLUS_EQUAL)
    addBanKind(set, Tokenizer.MINUS_EQUAL)
    addBanKind(set, Tokenizer.STAR_EQUAL)
    addBanKind(set, Tokenizer.SLASH_EQUAL)
    addBanKind(set, Tokenizer.PERCENT_EQUAL)
    addBanKind(set, Tokenizer.DOUBLE_EQUAL)
    addBanKind(set, Tokenizer.NOT_EQUAL)
    addBanKind(set, Tokenizer.GREATER_EQUAL)
    addBanKind(set, Tokenizer.LESS_EQUAL)
    addBanKind(set, Tokenizer.DOUBLE_PIPE)
    addBanKind(set, Tokenizer.DOUBLE_AMPERSAND)
    addBanKind(set, Tokenizer.DOUBLE_STAR)
    addBanKind(set, Tokenizer.DOUBLE_PLUS)
    addBanKind(set, Tokenizer.DOUBLE_MINUS)
    addBanKind(set, Tokenizer.ARROW)
    addBanKind(set, Tokenizer.EQUAL)
    addBanKind(set, Tokenizer.GREATER)
    addBanKind(set, Tokenizer.LESS)
    addBanKind(set, Tokenizer.CARET)
    addBanKind(set, Tokenizer.BANG)
    addBanKind(set, Tokenizer.PLUS)
    addBanKind(set, Tokenizer.MINUS)
    addBanKind(set, Tokenizer.STAR)
    addBanKind(set, Tokenizer.SLASH)
    addBanKind(set, Tokenizer.PERCENT)
    addBanKind(set, Tokenizer.LEFT_PAREN)
    addBanKind(set, Tokenizer.RIGHT_PAREN)
    addBanKind(set, Tokenizer.LEFT_BRACKET)
    addBanKind(set, Tokenizer.RIGHT_BRACKET)
    addBanKind(set, Tokenizer.LEFT_BRACE)
    addBanKind(set, Tokenizer.SEMICOLON)
    addBanKind(set, Tokenizer.COMMA)
    addBanKind(set, Tokenizer.COLON)
    addBanKind(set, Tokenizer.DOT)
}

private fun initBanAfter() -> pointer<HashSet>
{
    val result: pointer<HashSet> = new HashSet(sizeof(int), tokenKindCmp)

    addCommonBanKinds(result)

    return result
}

private fun initBanBefore() -> pointer<HashSet>
{
    val result: pointer<HashSet> = new HashSet(sizeof(int), tokenKindCmp)

    addCommonBanKinds(result)

    return result
}

private inline fun enterParenthesis(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    fsm.changeParenthesis(1)
    return false
}

private inline fun exitParenthesis(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    fsm.changeParenthesis(-1)
    return false
}

private inline fun enterBracket(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    fsm.changeBracket(1)
    return false
}

private inline fun exitBracket(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    fsm.changeBracket(-1)
    return false
}

private inline fun deleteLineTerminatorInPair(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    if !fsm.hasOpenPair():
        return false

    fsm.deleteToken()
    return true
}

private inline fun deleteCursor(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    fsm.deleteToken()
    return true
}

private fun insertLineTerminatorAroundRightBrace(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    if tokens == null || tokens.length != 3:
        return false

    val previous: pointer<Token> = tokens.get(0) as pointer<Token>
    val rightBrace: pointer<Token> = tokens.get(1) as pointer<Token>
    val next: pointer<Token> = tokens.get(2) as pointer<Token>
    var changed: bool = false

    if previous.kind != Tokenizer.TK_LINE_TERMINATOR:
    {
        val terminator: pointer<Token> = new Token(Tokenizer.TK_LINE_TERMINATOR, rightBrace.pos, null)
        fsm.insertToken(1, terminator)
        changed = true
    }

    if next.kind != Tokenizer.TK_LINE_TERMINATOR:
    {
        val terminator: pointer<Token> = new Token(Tokenizer.TK_LINE_TERMINATOR, next.pos, null)
        fsm.insertToken(2, terminator)
        changed = true
    }

    return changed
}

private fun isBanToken(set: pointer<HashSet>, token: pointer<Token>) -> bool
{
    if set == null || token == null:
        return false

    var kindSpace: int = token.kind
    val kindPtr: pointer<int> = kindSpace.ref

    return set.contains(kindPtr)
}

private fun deleteLineTerminatorBeforeBanToken(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    if tokens == null || tokens.length != 2:
        return false

    val next: pointer<Token> = tokens.get(1) as pointer<Token>

    if !isBanToken(banBefore, next):
        return false

    fsm.deleteToken()
    return true
}

private fun deleteLineTerminatorAfterBanToken(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    if tokens == null || tokens.length != 2:
        return false

    val previous: pointer<Token> = tokens.get(0) as pointer<Token>

    if !isBanToken(banAfter, previous):
        return false

    fsm.deleteToken()
    return true
}

fun canonicalize(tokenlist: pointer<TokenList>) -> pointer<TokenList>
{
    val canonical: pointer<TokenList> = new TokenList(tokenlist.filePath)
    var parenthesis: int = 0
    var bracket: int = 0

    for (var i = 0; i < tokenlist.length(); i++):
    {
        var token: pointer<Token> = tokenlist.get(i)

        if token.kind == Tokenizer.LEFT_PAREN:
            parenthesis++

        if token.kind == Tokenizer.RIGHT_PAREN:
        {
            if parenthesis > 0:
                parenthesis--
        }

        if token.kind == Tokenizer.LEFT_BRACKET:
            bracket++

        if token.kind == Tokenizer.RIGHT_BRACKET:
        {
            if bracket > 0:
                bracket--
        }

        if token.kind == Tokenizer.SEMICOLON:
        {
            if parenthesis == 0:
            {
                if bracket == 0:
                    token = new Token(Tokenizer.TK_LINE_TERMINATOR, token.pos, "\n")
            }
        }

        canonical.push(token)
    }

    val result: pointer<TokenList> = new TokenList(tokenlist.filePath)

    for (var i = 0; i < canonical.length(); i++):
    {
        val token: pointer<Token> = canonical.get(i)

        if token.isEOF():
        {
            if result.length() == 0:
            {
                val terminator: pointer<Token> = new Token(Tokenizer.TK_LINE_TERMINATOR, token.pos, "\n")
                result.push(terminator)
            }
            else:
            {
                val last: pointer<Token> = result.get(result.length() - 1)

                if last.kind != Tokenizer.TK_LINE_TERMINATOR:
                {
                    val terminator: pointer<Token> = new Token(Tokenizer.TK_LINE_TERMINATOR, token.pos, "\n")
                    result.push(terminator)
                }
            }

            result.push(token)
        }
        else:
        {
            if token.kind == Tokenizer.TK_LINE_TERMINATOR:
            {
                if result.length() > 0:
                    result.push(token)

                while i + 1 < canonical.length():
                {
                    val next: pointer<Token> = canonical.get(i + 1)

                    if next.kind != Tokenizer.TK_LINE_TERMINATOR:
                        break

                    i++
                }
            }
            else:
                result.push(token)
        }
    }

    return result
}


private fun normalizerInit()
{
    rulePtr[0] = new NormalizeRule(0, NormalizeFSM.DEFAULT, enterParenthesis).addPattern(Tokenizer.LEFT_PAREN).setPivot(0)
    rulePtr[1] = new NormalizeRule(1, NormalizeFSM.DEFAULT, exitParenthesis).addPattern(Tokenizer.RIGHT_PAREN).setPivot(0)
    rulePtr[2] = new NormalizeRule(2, NormalizeFSM.DEFAULT, enterBracket).addPattern(Tokenizer.LEFT_BRACKET).setPivot(0)
    rulePtr[3] = new NormalizeRule(3, NormalizeFSM.DEFAULT, exitBracket).addPattern(Tokenizer.RIGHT_BRACKET).setPivot(0)
    rulePtr[4] = new NormalizeRule(4, NormalizeFSM.DEFAULT, deleteLineTerminatorInPair).addPattern(Tokenizer.TK_LINE_TERMINATOR).setPivot(0)
    rulePtr[5] = new NormalizeRule(5, NormalizeFSM.DEFAULT, insertLineTerminatorAroundRightBrace).addPattern(Token.AnyKind).addPattern(Tokenizer.RIGHT_BRACE).addPattern(Token.AnyKind).setPivot(0)
    rulePtr[6] = new NormalizeRule(6, NormalizeFSM.DEFAULT, deleteLineTerminatorBeforeBanToken).addPattern(Tokenizer.TK_LINE_TERMINATOR).addPattern(Token.AnyKind).setPivot(0)
    rulePtr[7] = new NormalizeRule(7, NormalizeFSM.DEFAULT, deleteLineTerminatorAfterBanToken).addPattern(Token.AnyKind).addPattern(Tokenizer.TK_LINE_TERMINATOR).setPivot(1)
    normalizerIsInit = true
}


fun normalize(list: pointer<TokenList>) -> pointer<TokenList>
{
    if !normalizerIsInit:
        normalizerInit()

    val normalized: pointer<TokenList> = canonicalize(list)
    val fsm: pointer<NormalizeFSM> = new NormalizeFSM(normalized)
    val result: pointer<TokenList> = fsm.apply(rulePtr, ruleLength)

    return result
}


