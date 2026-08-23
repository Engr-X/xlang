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

@file.class("Test")
package xlang

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion

import xlang.lexer.NormalizeFSMTest
import xlang.lexer.TokenTest
import xlang.lexer.TokenizeFSMTest
import xlang.compiler.lexer.TokenizerTest
import xlang.compiler.lexer.TokenNormalizerTest
import xlang.compiler.parser.ParserTest
import xlang.parser.ParserUtilTest
import xlang.parser.PrattParserTest
import xlang.parser.RecursiveParserTest
import xlang.util.IO
import xlang.util.ArrayListTest
import xlang.util.HashSetTest
import xlang.util.TypeConvertTest
import xlang.util.string.StringBuilderTest
import xlang.util.string.StringTest


private fun getTestGroup() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang")
    val testGroupSpace: blob[sizeof(pointer<TestGroup>) * 100]
    val testGroupLength: int = 14
    val testGroups: pointer<pointer<TestGroup>> = testGroupSpace as pointer<pointer<TestGroup>>

    testGroups[0] = StringTest.TEST_GROUP
    testGroups[1] = StringBuilderTest.TEST_GROUP
    testGroups[2] = ArrayListTest.TEST_GROUP
    testGroups[3] = HashSetTest.TEST_GROUP
    testGroups[4] = TypeConvertTest.TEST_GROUP
    testGroups[5] = TokenTest.TEST_GROUP
    testGroups[6] = TokenizeFSMTest.TEST_GROUP
    testGroups[7] = NormalizeFSMTest.TEST_GROUP
    testGroups[8] = TokenizerTest.TEST_GROUP
    testGroups[9] = TokenNormalizerTest.TEST_GROUP
    testGroups[10] = ParserUtilTest.TEST_GROUP
    testGroups[11] = ParserTest.TEST_GROUP
    testGroups[12] = RecursiveParserTest.TEST_GROUP
    testGroups[13] = PrattParserTest.TEST_GROUP

    for (var i = 0; i < testGroupLength; i++):
    {
        val tu: pointer<TestUnion> = new TestUnion(TestGroup.TYPE, null, testGroups[i])
        result.addTestUnion(tu)
    }

    return result
}


fun main()
{
    IO.enableANSIColor()

    val testGroup: pointer<TestGroup> = getTestGroup()

    testGroup.runTest()
}

