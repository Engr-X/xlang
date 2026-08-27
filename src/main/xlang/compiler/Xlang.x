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
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.IO
import xlang.util.string.StringBuilder


fun main()
{
    parseStatementLoop()
}


private fun skipLineTerminators(tokens: pointer<TokenList>)
{
    while tokens != null && tokens.length() > 0:
    {
        val token: pointer<Token> = tokens.get(0)

        if token == null || token.kind != Tokenizer.TK_LINE_TERMINATOR:
            return

        tokens.remove(0, 1)
    }
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

        val tokens: pointer<TokenList> = Tokenizer.fullTokenize(input)
        val expression: pointer<Expression> = Parser.parseExpression(tokens)

        if expression == null:
        {
            putln("failed to parse expression")
            continue
        }

        // skipLineTerminators(tokens)

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


fun parseStatementLoop()
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

        if length + 1 >= 1024:
        {
            putln("failed to parse statement: input too long")
            continue
        }

        input[length] = '\n'
        input[length + 1] = 0

        val tokens: pointer<TokenList> = Tokenizer.fullTokenize(input)
        val statement: pointer<Statement> = Parser.parseStatement(tokens)

        if statement == null:
        {
            putln("failed to parse statement")
            continue
        }

        // skipLineTerminators(tokens)

        if tokens.length() > 0 && !tokens.get(0).isEOF():
        {
            val unexpected: pointer<Token> = tokens.get(0)
            val builder: pointer<StringBuilder> = new StringBuilder("failed to parse statement: unexpected token ")

            if unexpected != null && unexpected.text != null:
                builder.append(unexpected.text)
            else:
                builder.append("<unknown>")

            val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

            builder.toString(output)
            putln(output)
            continue
        }

        val builder: pointer<StringBuilder> = statement.toString()
        val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

        builder.toString(output)
        putln(output)
    }
}
