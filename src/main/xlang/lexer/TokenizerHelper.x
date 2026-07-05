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
 *
 *
 */

@file.class("TokenizerHelper")
package xlang.lexer


fun tokenize(code: pointer<char>, rules: pointer<pointer<Rule>>, rulesLength: int) -> TokenList
{
    val tokenList: TokenList = new TokenList()
    val lexState: LexState = LexState(code)

    while true:
    {
        val token: Token = lexState.apply(rules, rulesLength)

        if token == null:
            continue
            
        tokenList.push(token)

        if token.kind < 0 || token.isEOF():
            break
    }

    return tokenList
}
