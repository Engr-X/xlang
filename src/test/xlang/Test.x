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

import xlang.util.IO
import xlang.util.string.StringBuilderTest
import xlang.util.string.StringTest


private fun getTestGroup() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang" as pointer<char>)
    val testGroupSpace: blob[sizeof(pointer<TestGroup>) * 100]
    val testGroupLength: int = 2
    val testGroups: pointer<pointer<TestGroup>> = testGroupSpace as pointer<pointer<TestGroup>>

    testGroups[0] = StringTest.TEST_GROUP
    testGroups[1] = StringBuilderTest.TEST_GROUP

    for (var i = 0; i < testGroupLength; i++)
    {
        val tu: TestUnion = new TestUnion(TestGroup.TYPE, null, testGroups[i])
        result.addTestUnion(tu)
    }

    return result
}


fun main()
{
    IO.enableANSIColor()

    val testGroup: TestGroup = getTestGroup()

    testGroup.runTest()
}
