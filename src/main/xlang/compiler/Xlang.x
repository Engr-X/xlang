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

import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.lexer.TokenNormalizer
import xlang.lexer.TokenList
import xlang.util.IO
import xlang.util.string.StringBuilder


fun main()
{
    val text: pointer<char> = IO.readFile("D:/Coding/projects/Xlang/xlang/src/main/xlang/compiler/TestInput.test")

    if text == null:
    {
        putln("failed to read input file")
        return
    }

    val raw: pointer<TokenList> = Tokenizer.fullTokenize(text)
    val builder: pointer<StringBuilder> = raw.toString(Tokenizer.TK_LINE_TERMINATOR)
    val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

    builder.toString(output)
    putln(output)
}
