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

@file.class("Xlang")
package xlang.compiler

import xlang.compiler.lex.Tokenizer
import xlang.lex.Token
import xlang.lex.TokenList


fun main()
{
    val tokens: TokenList = Tokenizer.tokenize("111 Hello++ \"string\r\n\", this is my programming language" as pointer<char>)

    for (var i: int = 0; i < tokens.length(); i++):
    {
        val token: pointer<Token> = tokens.get(i)

        if token.kind == Token.EOF_KIND:
            putln("<EOF>" as pointer<char>)
        else:
            putln(token.text)
    }
}
