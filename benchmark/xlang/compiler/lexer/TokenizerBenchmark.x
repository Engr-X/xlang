/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 */

#file.outerClass("TokenizerBenchmark")
package xlang.compiler.lexer

import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.lexer.TokenNormalizer
import xlang.lexer.TokenList
import xlang.util.IO
import xlang.util.string.String


private val SOURCE_PATH: pointer<char> =
    "D:/Coding/projects/Xlang/xlang/src/main/xlang/compiler/lexer/Tokenizer.x"

private val REGEX_ITERATIONS: int = 20000


private fun printTiming(name: pointer<char>, elapsed: long)
{
    put(name)
    put(": ")
    putln(elapsed)
}


private fun benchmarkRegex(
    name: pointer<char>, pattern: pointer<char>, text: pointer<char>)
{
    var checksum: int = 0
    val start: long = System.nowNs()

    for (var i = 0; i < REGEX_ITERATIONS; i++):
        checksum += String.strRegMatch(pattern, text)

    val end: long = System.nowNs()
    printTiming(name, end - start)
    put("regex checksum: ")
    putln(checksum)
}


fun main()
{
    var start: long = System.nowNs()
    val source: pointer<char> = IO.readFile(SOURCE_PATH)
    var end: long = System.nowNs()
    printTiming("read file ns", end - start)

    if source == null:
    {
        putln("failed to read benchmark source")
        return
    }

    start = System.nowNs()
    val coldTokens: pointer<TokenList> = Tokenizer.tokenize(source, SOURCE_PATH)
    end = System.nowNs()
    printTiming("tokenize cold ns", end - start)

    start = System.nowNs()
    val warmTokens: pointer<TokenList> = Tokenizer.tokenize(source, SOURCE_PATH)
    end = System.nowNs()
    printTiming("tokenize warm ns", end - start)

    start = System.nowNs()
    val canonicalTokens: pointer<TokenList> = TokenNormalizer.canonicalize(warmTokens)
    end = System.nowNs()
    printTiming("canonicalize ns", end - start)

    start = System.nowNs()
    val normalizedTokens: pointer<TokenList> = TokenNormalizer.normalize(warmTokens)
    end = System.nowNs()
    printTiming("normalize total ns", end - start)

    start = System.nowNs()
    val fullTokens: pointer<TokenList> = Tokenizer.fullTokenize(source, SOURCE_PATH)
    end = System.nowNs()
    printTiming("fullTokenize ns", end - start)

    put("cold token count: ")
    putln(coldTokens.length())
    put("canonical token count: ")
    putln(canonicalTokens.length())
    put("normalized token count: ")
    putln(normalizedTokens.length())
    put("full token count: ")
    putln(fullTokens.length())

    benchmarkRegex("regex literal ns", ">>>=", ">>>=rest")
    benchmarkRegex(
        "regex identifier ns",
        "[a-zA-Z_][a-zA-Z0-9_]*",
        "identifier123 rest")
    benchmarkRegex(
        "regex integer ns",
        "0[xX][0-9a-fA-F]+|[0-9]+",
        "123456 rest")
}
