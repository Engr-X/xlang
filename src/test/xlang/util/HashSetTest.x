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
 */

@file.class("HashSetTest")
package xlang.util

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.util.HashSet")
    val basicTC: pointer<TestCase> = new TestCase("basic", basicTest)
    val removeTC: pointer<TestCase> = new TestCase("remove", removeTest)
    val toArrayTC: pointer<TestCase> = new TestCase("toArray", toArrayTest)
    val chainAddTC: pointer<TestCase> = new TestCase("chainAdd", chainAddTest)

    val basicUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, basicTC, null)
    val removeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, removeTC, null)
    val toArrayUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, toArrayTC, null)
    val chainAddUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, chainAddTC, null)

    result.addTestUnion(basicUnion)
    result.addTestUnion(removeUnion)
    result.addTestUnion(toArrayUnion)
    result.addTestUnion(chainAddUnion)

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


private fun basicTest() -> int
{
    val set: pointer<HashSet> = new HashSet(sizeof(int), intCmp)
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 10

    if set.contains(value):
        return 1

    if !set.addIfAbsent(value):
        return 2

    if set.length != 1:
        return 3

    if !set.contains(value):
        return 4

    if set.addIfAbsent(value):
        return 5

    if set.length != 1:
        return 6

    value.deref = 20

    if !set.addIfAbsent(value):
        return 7

    if set.length != 2:
        return 8

    return 0
}


private fun chainAddTest() -> int
{
    val set: pointer<HashSet> = new HashSet(sizeof(int), intCmp)
    val valueSpace: blob[sizeof(int) * 3]
    val values: pointer<int> = valueSpace as pointer<int>

    values[0] = 7
    values[1] = 8
    values[2] = 7

    val chained: pointer<HashSet> = set.add(values).add(values + 1).add(values + 2)

    if chained != set:
        return 1

    if set.length != 2:
        return 2

    if !set.contains(values):
        return 3

    if !set.contains(values + 1):
        return 4

    return 0
}


private fun removeTest() -> int
{
    val set: pointer<HashSet> = new HashSet(sizeof(int), 2, 0.75, intCmp)
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 1
    set.add(value)

    value.deref = 2
    set.add(value)

    value.deref = 3
    set.add(value)

    if set.length != 3:
        return 1

    value.deref = 2

    if !set.remove(value):
        return 2

    if set.length != 2:
        return 3

    if set.contains(value):
        return 4

    if set.remove(value):
        return 5

    value.deref = 1

    if !set.contains(value):
        return 6

    value.deref = 3

    if !set.contains(value):
        return 7

    return 0
}


private fun toArrayTest() -> int
{
    val set: pointer<HashSet> = new HashSet(sizeof(int), intCmp)
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 4
    set.add(value)

    value.deref = 6
    set.add(value)

    val array: pointer<ArrayList> = set.toArray()

    if array == null:
        return 1

    if array.length != 2:
        return 2

    if (array.get(0) as pointer<int>).deref != 4:
        return 3

    if (array.get(1) as pointer<int>).deref != 6:
        return 4

    value.deref = 8
    set.add(value)

    if array.length != 2:
        return 5

    return 0
}
