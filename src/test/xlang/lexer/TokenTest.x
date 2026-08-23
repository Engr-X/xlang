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

@file.class("TokenTest")
package xlang.lexer

import xlang.lexer.TokenPosition
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.lexer.Token")
    val compareTC: pointer<TestCase> = new TestCase("positionCompare", positionCompareTest)
    val compareUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, compareTC, null)

    result.addTestUnion(compareUnion)

    return result
}


private fun positionCompareTest() -> int
{
    val same0: pointer<TokenPosition> = new TokenPosition(1, 1, 2, 3)
    val same1: pointer<TokenPosition> = new TokenPosition(1, 1, 2, 3)
    val laterOffset: pointer<TokenPosition> = new TokenPosition(2, 1, 1, 1)
    val laterLine: pointer<TokenPosition> = new TokenPosition(1, 2, 1, 1)
    val laterColumn: pointer<TokenPosition> = new TokenPosition(1, 1, 3, 1)
    val laterLength: pointer<TokenPosition> = new TokenPosition(1, 1, 2, 4)

    if TokenPosition.compare(null, null) != 0:
        return 1

    if TokenPosition.compare(null, same0) >= 0:
        return 2

    if TokenPosition.compare(same0, null) <= 0:
        return 3

    if TokenPosition.compare(same0, same1) != 0:
        return 4

    if TokenPosition.compare(same0, laterOffset) >= 0:
        return 5

    if TokenPosition.compare(laterOffset, same0) <= 0:
        return 6

    if TokenPosition.compare(same0, laterLine) >= 0:
        return 7

    if TokenPosition.compare(same0, laterColumn) >= 0:
        return 8

    if TokenPosition.compare(same0, laterLength) >= 0:
        return 9

    return 0
}