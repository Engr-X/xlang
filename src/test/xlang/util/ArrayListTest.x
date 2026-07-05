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
 */

@file.class("ArrayListTest")
package xlang.util

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.String


val TEST_GROUP: TestGroup = genTest()


fun genTest() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang.util.ArrayList")

    val pushGetTC: pointer<TestCase> = new TestCase("pushGet", pushGetTest)
    val pushFrontTC: pointer<TestCase> = new TestCase("pushFront", pushFrontTest)
    val setTC: pointer<TestCase> = new TestCase("set", setTest)
    val resizeTC: pointer<TestCase> = new TestCase("resize", resizeTest)
    val pointerElementTC: pointer<TestCase> = new TestCase("pointerElement", pointerElementTest)
    val addTC: pointer<TestCase> = new TestCase("add", addTest)
    val addAllTC: pointer<TestCase> = new TestCase("addAll", addAllTest)
    val removeAtTC: pointer<TestCase> = new TestCase("removeAt", removeAtTest)
    val indexOfTC: pointer<TestCase> = new TestCase("indexOf", indexOfTest)
    val removeTC: pointer<TestCase> = new TestCase("remove", removeTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 16]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 10

    testCase[0] = pushGetTC
    testCase[1] = pushFrontTC
    testCase[2] = setTC
    testCase[3] = resizeTC
    testCase[4] = pointerElementTC
    testCase[5] = addTC
    testCase[6] = addAllTC
    testCase[7] = removeAtTC
    testCase[8] = indexOfTC
    testCase[9] = removeTC

    for (var i = 0; i < testCaseLength; i++)
    {
        val tu: TestUnion = new TestUnion(TestCase.TYPE, testCase[i], null)
        result.addTestUnion(tu)
    }

    return result
}


private fun intCmp(left: pointer<*>, right: pointer<*>) -> int
{
    val lhs: int = (left as pointer<int>).deref
    val rhs: int = (right as pointer<int>).deref

    if lhs == rhs:
        return 0

    return if lhs < rhs:
        -1
    else:
        1
}


private fun pushGetTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 10
    list.push(value)

    value.deref = 20
    list.push(value)

    value.deref = 30
    list.push(value)

    if list.length != 3:
        return 1

    if (list.get(0) as pointer<int>).deref != 10:
        return 2

    if (list.get(1) as pointer<int>).deref != 20:
        return 3

    if (list.get(2) as pointer<int>).deref != 30:
        return 4

    value.deref = 99

    if (list.get(1) as pointer<int>).deref != 20:
        return 5

    return 0
}


private fun addTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int), 2, 0.75)
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 10
    list.push(value)

    value.deref = 30
    list.push(value)

    value.deref = 20
    list.add(1, value)

    value.deref = 5
    list.add(0, value)

    value.deref = 40
    list.add(list.length, value)

    value.deref = 99
    list.add(-1, value)
    list.add(list.length + 1, value)

    if list.length != 5:
        return 1

    if (list.get(0) as pointer<int>).deref != 5:
        return 2

    if (list.get(1) as pointer<int>).deref != 10:
        return 3

    if (list.get(2) as pointer<int>).deref != 20:
        return 4

    if (list.get(3) as pointer<int>).deref != 30:
        return 5

    if (list.get(4) as pointer<int>).deref != 40:
        return 6

    return 0
}


private fun addAllTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int), 2, 0.75)
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>
    val sourceSpace: blob[sizeof(int) * 4]
    val source: pointer<int> = sourceSpace as pointer<int>
    val prefixSpace: blob[sizeof(int)]
    val prefix: pointer<int> = prefixSpace as pointer<int>
    val middleSpace: blob[sizeof(int) * 2]
    val middle: pointer<int> = middleSpace as pointer<int>

    value.deref = 1
    list.push(value)

    source[0] = 2
    source[1] = 3
    source[2] = 4
    source[3] = 5

    list.addAll(list.length, source, 4)

    prefix[0] = 0
    list.addAll(0, prefix, 1)

    middle[0] = 99
    middle[1] = 100
    list.addAll(3, middle, 2)

    list.addAll(0, source, 0)
    list.addAll(0, source, -1)
    list.addAll(-1, source, 1)
    list.addAll(list.length + 1, source, 1)

    if list.length != 8:
        return 1

    if (list.get(0) as pointer<int>).deref != 0:
        return 2

    if (list.get(1) as pointer<int>).deref != 1:
        return 3

    if (list.get(2) as pointer<int>).deref != 2:
        return 4

    if (list.get(3) as pointer<int>).deref != 99:
        return 5

    if (list.get(4) as pointer<int>).deref != 100:
        return 6

    if (list.get(5) as pointer<int>).deref != 3:
        return 7

    if (list.get(6) as pointer<int>).deref != 4:
        return 8

    if (list.get(7) as pointer<int>).deref != 5:
        return 9

    source[1] = 99

    if (list.get(5) as pointer<int>).deref != 3:
        return 10

    return 0
}


private fun removeAtTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    for (var i = 0; i < 5; i++)
    {
        value.deref = i + 1
        list.push(value)
    }

    list.removeAt(0)
    list.removeAt(2)
    list.removeAt(2)
    list.removeAt(-1)
    list.removeAt(list.length)

    if list.length != 2:
        return 1

    if (list.get(0) as pointer<int>).deref != 2:
        return 2

    if (list.get(1) as pointer<int>).deref != 3:
        return 3

    return 0
}


private fun indexOfTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 11
    list.push(value)

    value.deref = 22
    list.push(value)

    value.deref = 33
    list.push(value)

    value.deref = 22
    list.push(value)

    value.deref = 22
    if list.indexOf(value, intCmp) != 1:
        return 1

    value.deref = 11
    if list.indexOf(value, intCmp) != 0:
        return 2

    value.deref = 44
    if list.indexOf(value, intCmp) != -1:
        return 3

    return 0
}


private fun removeTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 7
    list.push(value)

    value.deref = 8
    list.push(value)

    value.deref = 9
    list.push(value)

    value.deref = 8
    list.push(value)

    value.deref = 8
    list.remove(value, intCmp)

    if list.length != 3:
        return 1

    if (list.get(0) as pointer<int>).deref != 7:
        return 2

    if (list.get(1) as pointer<int>).deref != 9:
        return 3

    if (list.get(2) as pointer<int>).deref != 8:
        return 4

    list.remove(value, intCmp)

    if list.length != 2:
        return 5

    if (list.get(0) as pointer<int>).deref != 7:
        return 6

    if (list.get(1) as pointer<int>).deref != 9:
        return 7

    value.deref = 42
    list.remove(value, intCmp)

    if list.length != 2:
        return 8

    return 0
}


private fun pushFrontTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 1
    list.push(value)

    value.deref = 2
    list.push(value)

    value.deref = 3
    list.push(value)

    value.deref = 0
    list.pushFront(value)

    if list.length != 4:
        return 1

    if (list.get(0) as pointer<int>).deref != 0:
        return 2

    if (list.get(1) as pointer<int>).deref != 1:
        return 3

    if (list.get(2) as pointer<int>).deref != 2:
        return 4

    if (list.get(3) as pointer<int>).deref != 3:
        return 5

    return 0
}


private fun setTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 1
    list.push(value)

    value.deref = 2
    list.push(value)

    value.deref = 3
    list.push(value)

    value.deref = 42
    list.set(1, value)
    list.set(-1, value)
    list.set(3, value)

    if list.length != 3:
        return 1

    if (list.get(0) as pointer<int>).deref != 1:
        return 2

    if (list.get(1) as pointer<int>).deref != 42:
        return 3

    if (list.get(2) as pointer<int>).deref != 3:
        return 4

    return 0
}


private fun resizeTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(int), 2, 0.75)
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    for (var i = 0; i < 6; i++)
    {
        value.deref = i * 3 + 1
        list.push(value)
    }

    if list.length != 6:
        return 1

    for (var i = 0; i < 6; i++)
    {
        val expected: int = i * 3 + 1

        if (list.get(i) as pointer<int>).deref != expected:
            return i + 2
    }

    return 0
}


private fun pointerElementTest() -> int
{
    val list: ArrayList = new ArrayList(sizeof(pointer<char>))
    val sourceSpace: blob[sizeof(pointer<char>)]
    val source: pointer<pointer<char>> = sourceSpace as pointer<pointer<char>>

    source.deref = "alpha"
    list.push(source)

    source.deref = "beta"
    list.push(source)

    source.deref = "gamma"
    list.set(1, source)

    if list.length != 2:
        return 1

    if !String.streq((list.get(0) as pointer<pointer<char>>).deref, "alpha"):
        return 2

    if !String.streq((list.get(1) as pointer<pointer<char>>).deref, "gamma"):
        return 3

    source.deref = "changed"

    if !String.streq((list.get(0) as pointer<pointer<char>>).deref, "alpha"):
        return 4

    return 0
}
