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
 */

@file.class("Xlang")
package xlang.compiler

import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.Parser
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.IO
import xlang.util.string.StringBuilder


fun main()
{
    parseExpressionLoop()
}


fun parseExpressionLoop()
{
    val inputSpace: blob[1024]
    val input: pointer<char> = inputSpace as pointer<char>

    while true:
    {
        val length: int = IO.readLine(input, 1024)

        if length < 0:
            return

        if length == 0:
            continue

        val tokens: pointer<TokenList> = Tokenizer.tokenize(input)
        val expression: pointer<Expression> = Parser.parseExpression(tokens)

        if expression == null:
        {
            putln("failed to parse expression")
            continue
        }

        if tokens.length() > 0 && !tokens.get(0).isEOF():
        {
            val unexpected: pointer<Token> = tokens.get(0)
            val builder: pointer<StringBuilder> = new StringBuilder("failed to parse expression: unexpected token ")

            if unexpected != null && unexpected.text != null:
                builder.append(unexpected.text)
            else:
                builder.append("<unknown>")

            val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

            builder.toString(output)
            putln(output)
            continue
        }

        val builder: pointer<StringBuilder> = expression.toString()
        val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

        builder.toString(output)
        putln(output)
    }
}