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

import xlang.compiler.lexer.Tokenizer
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.IO


fun main()
{
    val text: pointer<char> = IO.readFile("D:/Coding/projects/Xlang/xlang/src/main/xlang/compiler/TestInput.test")
    val tokens: TokenList = Tokenizer.tokenize(text)

    for (var i: int = 0; i < tokens.length(); i++):
    {
        val token: pointer<Token> = tokens.get(i)

        if token.kind == Token.EOF_KIND:
            putln("<EOF>")
        elif token.kind < 0:
            putln(token.errorInfo)
        elif token.text == null:
            pass
        else:
            putln(token.text)
    }
}
