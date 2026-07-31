/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package xlang.lexer

import xlang.System
import xlang.util.string.String
import xlang.util.ArrayList


private fun noopNormalizeAction(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool =
    false


struct NormalizeReceiver
{
    var length: int
    private var deleted: pointer<bool>
    private var addedTokens: pointer<pointer<Token>>


    fun __init__(length: int)
    {
        if length <= 0:
        {
            this.length = 0
            this.deleted = null
            this.addedTokens = null
            return
        }

        this.length = length
        this.deleted = System.allocMemory(length * sizeof(bool)) as pointer<bool>
        this.addedTokens = System.allocMemory(length * sizeof(pointer<Token>)) as pointer<pointer<Token>>

        for (var i = 0; i < length; i++):
        {
            this.deleted[i] = false
            this.addedTokens[i] = null
        }
    }


    private inline fun checkIndex(index: int) -> bool =
        0 <= index && index < this.length


    fun deleteAt(index: int) -> pointer<NormalizeReceiver>
    {
        if !this.checkIndex(index):
            return this

        this.deleted[index] = true
        return this
    }


    fun insertAt(index: int, token: pointer<Token>) -> pointer<NormalizeReceiver>
    {
        if !this.checkIndex(index):
            return this

        this.addedTokens[index] = token
        return this
    }


    fun apply(list: pointer<TokenList>) -> pointer<TokenList>
    {
        if list == null || list.length() != this.length:
            return null

        val result: pointer<TokenList> = new TokenList()
        var deleteCursor: pointer<bool> = this.deleted
        var addCursor: pointer<pointer<Token>> = this.addedTokens

        for (var i = 0; i < this.length; i++):
        {
            val current: pointer<Token> = list.get(i)

            if addCursor.deref != null:
                result.push(addCursor.deref)

            if !deleteCursor.deref:
                result.push(current)

            deleteCursor++
            addCursor++
        }

        return result
    }
}


struct TokenPattern
{
    var kind: int
    var pattern: pointer<char>


    fun __init__(kind: int, pattern: pointer<char>)
    {
        this.kind = kind
        this.pattern = pattern
    }


    private inline fun matchKind(token: pointer<Token>) -> bool =
        this.kind == Token.AnyKind || this.kind == token.kind


    fun match(token: pointer<Token>) -> bool
    {
        if !this.matchKind(token):
            return false

        if this.pattern == null:
            return true

        if token.text == null:
            return false

        return String.strRegMatch(this.pattern, token.text) > 0
    }
}


struct NormalizeRule
{
    var id: int
    var state: int
    var action: (pointer<NormalizeFSM>, pointer<ArrayList>) -> bool
    
    // ArrayList of StringBuilder
    private var pivot: pointer<TokenPattern>

    // this is relative index.
    private var pivotIndex: int
    
    private var patterns: pointer<ArrayList>


    fun __init__(id: int, state: int):
    {
        this.id = id
        this.state = state
        this.action = noopNormalizeAction
        this.pivot = null
        this.pivotIndex = -1
        this.patterns = new ArrayList(sizeof(TokenPattern))
    }


    fun __init__(id: int, state: int, action: (pointer<NormalizeFSM>, pointer<ArrayList>) -> bool):
    {
        this.id = id
        this.state = state
        this.action = action
        this.pivot = null
        this.pivotIndex = -1
        this.patterns = new ArrayList(sizeof(TokenPattern))
    }


    fun addPattern(kind: int) -> pointer<NormalizeRule> =
        this.addPattern(kind, null)


    fun addPattern(kind: int, pattern: pointer<char>) -> pointer<NormalizeRule>
    {
        val rule: TokenPattern = TokenPattern(kind, pattern)
        this.patterns.push(rule.ref)
        return this
    }


    fun setPivot(pivot: int) -> pointer<NormalizeRule>
    {
        if pivot < 0 || pivot >= this.patterns.length:
        {
            this.pivot = null
            this.pivotIndex = -1
            return this
        }

        val patternSlot: pointer<TokenPattern> = this.patterns.get(pivot) as pointer<TokenPattern>
        this.pivotIndex = pivot
        this.pivot = patternSlot
        return this
    }


    fun match(tokens: pointer<ArrayList>, currentIndex: int) -> bool
    {
        if tokens == null || this.pivotIndex < 0:
            return false

        if currentIndex < 0 || currentIndex >= tokens.length:
            return false

        val current: pointer<Token> = tokens.get(currentIndex) as pointer<Token>
        val start: int = currentIndex - this.pivotIndex

        if start < 0:
            return false

        if start + this.patterns.length > tokens.length:
            return false

        if !this.pivot.match(current):
            return false

        for (var i = 0; i < this.patterns.length; i++):
        {
            val pattern: pointer<TokenPattern> = this.patterns.get(i) as pointer<TokenPattern>
            val token: pointer<Token> = tokens.get(start + i) as pointer<Token>

            if !pattern.match(token):
                return false
        }

        return true
    }


    fun apply(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool =
        this.action(fsm, tokens)


    fun getPivotIndex() -> int = this.pivotIndex


    fun getPatternLength() -> int = this.patterns.length
}


struct NormalizeFSM
{
    static val DEFAULT: int = 1

    var state: int

    private var currentIndex: int
    private var ptr: int
    private var list: pointer<TokenList>
    private var receiver: pointer<NormalizeReceiver>

    private var parenthesis: int
    private var bracket: int


    fun __init__(list: pointer<TokenList>)
    {
        this.state = DEFAULT
        this.currentIndex = 0
        this.ptr = 0
        this.list = list
        this.receiver = new NormalizeReceiver(list.length())

        this.parenthesis = 0
        this.bracket = 0
    }


    fun setState(state: int):
        this.state = state


    fun getState() -> int = this.state


    fun getCurrentIndex() -> int = this.currentIndex


    fun getPtr() -> int = this.ptr


    inline fun hasOpenPair() -> bool =
        this.parenthesis > 0 || this.bracket > 0


    static fun window(rule: pointer<NormalizeRule>, tokens: pointer<ArrayList>, currentIndex: int) -> pointer<ArrayList>
    {
        if rule == null || tokens == null:
            return null

        val start: int = currentIndex - rule.getPivotIndex()
        return tokens.sublist(start, start + rule.getPatternLength())
    }


    private fun initReceiver(rules: pointer<pointer<NormalizeRule>>, rulesLength: int)
    {
        if rules == null || rulesLength <= 0:
            return

        val tokenLength: int = this.list.length()
        val tokenList: pointer<ArrayList> = this.list.array()

        this.currentIndex = 0
        this.ptr = 0

        while this.currentIndex < tokenLength:
        {
            for (var j = 0; j < rulesLength; j++):
            {
                val rule: pointer<NormalizeRule> = rules[j]

                if rule == null:
                    continue

                if rule.state != this.state:
                    continue

                if !rule.match(tokenList, this.currentIndex):
                    continue

                val tokens: pointer<ArrayList> = NormalizeFSM.window(rule, tokenList, this.currentIndex)

                if rule.apply(this, tokens):
                {
                    this.ptr++
                    break
                }
            }

            this.currentIndex++
        }
    }


    fun apply(rules: pointer<pointer<NormalizeRule>>, rulesLength: int) -> pointer<TokenList>
    {
        this.initReceiver(rules, rulesLength)
        return this.receiver.apply(this.list)
    }


    fun deleteToken()
    {
        this.receiver.deleteAt(this.currentIndex)
    }

    
    fun insertToken(token: pointer<Token>)
    {
        this.receiver.insertAt(this.currentIndex, token)
    }


    fun insertToken(offset: int, token: pointer<Token>)
    {
        this.receiver.insertAt(this.currentIndex + offset, token)
    }


    inline fun changeParenthesis(offset: int):
        this.parenthesis += offset


    inline fun changeBracket(offset: int):
        this.bracket += offset
}
