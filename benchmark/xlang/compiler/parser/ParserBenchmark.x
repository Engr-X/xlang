/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 */

#file.outerClass("ParserBenchmark")
package xlang.compiler.parser

import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.Parser
import xlang.compiler.parser.program.Program
import xlang.lexer.TokenList
import xlang.util.IO


private val SOURCE_PATH: pointer<char> =
    "D:/Coding/projects/Xlang/xlang/src/main/xlang/compiler/parser/Parser.x"


private fun printTiming(name: pointer<char>, elapsed: long)
{
    put(name)
    put(": ")
    putln(elapsed)
}


private fun benchmarkParse(source: pointer<char>, path: pointer<char>, name: pointer<char>)
{
    var start: long = System.nowNs()
    val tokens: pointer<TokenList> = Tokenizer.fullTokenize(source, path)
    var end: long = System.nowNs()

    put(name)
    put(" token count: ")
    putln(tokens.length())
    printTiming("fullTokenize ns", end - start)

    start = System.nowNs()
    val program: pointer<Program> = Parser.parseProgram(tokens)
    end = System.nowNs()

    printTiming("parseProgram ns", end - start)

    if program == null:
        putln("parseProgram failed")
    else:
    {
        put("remaining token count: ")
        putln(tokens.length())
    }
}


fun main()
{
    val source: pointer<char> = IO.readFile(SOURCE_PATH)

    if source == null:
        return

    put("parse fresh ")
    putln(SOURCE_PATH)
    benchmarkParse(source, SOURCE_PATH, "file")
}
