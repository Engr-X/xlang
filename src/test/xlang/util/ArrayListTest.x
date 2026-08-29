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


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.util.ArrayList")

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
    val sublistTC: pointer<TestCase> = new TestCase("sublist", sublistTest)
    val peekPopTC: pointer<TestCase> = new TestCase("peekPop", peekPopTest)
    val comparatorTC: pointer<TestCase> = new TestCase("comparator", comparatorTest)
    val sortTC: pointer<TestCase> = new TestCase("sort", sortTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 16]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 14

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
    testCase[10] = sublistTC
    testCase[11] = peekPopTC
    testCase[12] = comparatorTC
    testCase[13] = sortTC

    for (var i = 0; i < testCaseLength; i++):
    {
        val tu: pointer<TestUnion> = new TestUnion(TestCase.TYPE, testCase[i], null)
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
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
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
    val list: pointer<ArrayList> = new ArrayList(sizeof(int), 2, 0.75)
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
    val list: pointer<ArrayList> = new ArrayList(sizeof(int), 2, 0.75)
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
    val target: pointer<ArrayList> = new ArrayList(sizeof(int))
    val sourceList: pointer<ArrayList> = new ArrayList(sizeof(int))
    val badSource: pointer<ArrayList> = new ArrayList(sizeof(long))

    value.deref = 1
    target.push(value)
    value.deref = 4
    target.push(value)

    value.deref = 2
    sourceList.push(value)
    value.deref = 3
    sourceList.push(value)

    target.addAll(1, sourceList)

    if target.length != 4:
        return 11

    if (target.get(0) as pointer<int>).deref != 1:
        return 12

    if (target.get(1) as pointer<int>).deref != 2:
        return 13

    if (target.get(2) as pointer<int>).deref != 3:
        return 14

    if (target.get(3) as pointer<int>).deref != 4:
        return 15

    target.addAll(1, badSource)

    if target.length != 4:
        return 16

    target.addAll(-1, sourceList)

    if target.length != 4:
        return 17

    target.addAll(target.length + 1, sourceList)

    if target.length != 4:
        return 18

    target.addAll(target.length, null)

    if target.length != 4:
        return 19
    return 0
}


private fun removeAtTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    for (var i = 0; i < 5; i++):
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
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    list.setComparator(intCmp)

    value.deref = 11
    list.push(value)

    value.deref = 22
    list.push(value)

    value.deref = 33
    list.push(value)

    value.deref = 22
    list.push(value)

    value.deref = 22
    if list.indexOf(value) != 1:
        return 1

    value.deref = 11
    if list.indexOf(value) != 0:
        return 2

    value.deref = 44
    if list.indexOf(value) != -1:
        return 3

    return 0
}


private fun removeTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    list.setComparator(intCmp)

    value.deref = 7
    list.push(value)

    value.deref = 8
    list.push(value)

    value.deref = 9
    list.push(value)

    value.deref = 8
    list.push(value)

    value.deref = 8
    list.remove(value)

    if list.length != 3:
        return 1

    if (list.get(0) as pointer<int>).deref != 7:
        return 2

    if (list.get(1) as pointer<int>).deref != 9:
        return 3

    if (list.get(2) as pointer<int>).deref != 8:
        return 4

    list.remove(value)

    if list.length != 2:
        return 5

    if (list.get(0) as pointer<int>).deref != 7:
        return 6

    if (list.get(1) as pointer<int>).deref != 9:
        return 7

    value.deref = 42
    list.remove(value)

    if list.length != 2:
        return 8

    return 0
}


private fun comparatorTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    value.deref = 4
    list.push(value)

    value.deref = 5
    list.push(value)

    value.deref = 6
    list.push(value)

    value.deref = 5

    if list.indexOf(value) != -1:
        return 1

    if list.contains(value):
        return 2

    list.remove(value)

    if list.length != 3:
        return 3

    list.setComparator(intCmp)

    if list.indexOf(value) != 1:
        return 4

    if !list.contains(value):
        return 5

    list.remove(value)

    if list.length != 2:
        return 6

    if (list.get(0) as pointer<int>).deref != 4:
        return 7

    if (list.get(1) as pointer<int>).deref != 6:
        return 8

    value.deref = 6

    val clone: pointer<ArrayList> = list.sublist(0, list.length)

    if clone == null:
        return 9

    if !clone.contains(value):
        return 10

    value.deref = 4

    if clone.indexOf(value) != 0:
        return 11

    val seeded: pointer<ArrayList> = new ArrayList(sizeof(int), 2, 0.75, intCmp)

    value.deref = 12
    seeded.push(value)

    value.deref = 13
    seeded.push(value)

    value.deref = 12

    if !seeded.contains(value):
        return 12

    if seeded.indexOf(value) != 0:
        return 13

    if !seeded.contains(value):
        return 14

    return 0
}


private fun sortTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    list.setComparator(intCmp)

    value.deref = 4
    list.push(value)
    value.deref = 1
    list.push(value)
    value.deref = 3
    list.push(value)
    value.deref = 2
    list.push(value)
    value.deref = 3
    list.push(value)

    list.sort()

    if list.length != 5:
        return 1

    val expectedSpace: blob[sizeof(int) * 5]
    val expected: pointer<int> = expectedSpace as pointer<int>

    expected[0] = 1
    expected[1] = 2
    expected[2] = 3
    expected[3] = 3
    expected[4] = 4

    for (var i = 0; i < list.length; i++):
    {
        if (list.get(i) as pointer<int>).deref != expected[i]:
            return i + 2
    }

    return 0
}


private fun sublistTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    for (var i = 0; i < 6; i++):
    {
        value.deref = i + 1
        list.push(value)
    }

    val middle: pointer<ArrayList> = list.sublist(2, 5)

    if middle == null:
        return 1

    if middle.length != 3:
        return 2

    if (middle.get(0) as pointer<int>).deref != 3:
        return 3

    if (middle.get(1) as pointer<int>).deref != 4:
        return 4

    if (middle.get(2) as pointer<int>).deref != 5:
        return 5

    val empty: pointer<ArrayList> = list.sublist(6, 6)

    if empty == null:
        return 6

    if empty.length != 0:
        return 7

    if list.sublist(-1, 2) != null:
        return 8

    if list.sublist(4, 3) != null:
        return 9

    if list.sublist(0, 7) != null:
        return 10

    value.deref = 99
    list.set(2, value)

    if (middle.get(0) as pointer<int>).deref != 3:
        return 11

    value.deref = 88
    middle.set(1, value)

    if (list.get(3) as pointer<int>).deref != 4:
        return 12

    return 0
}


private fun peekPopTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    if list.peek() != null:
        return 1

    if list.peekFront() != null:
        return 2

    if list.pop() != null:
        return 3

    if list.popFront() != null:
        return 4

    for (var i = 0; i < 3; i++):
    {
        value.deref = i + 1
        list.push(value)
    }

    if (list.peek() as pointer<int>).deref != 3:
        return 5

    if (list.peekFront() as pointer<int>).deref != 1:
        return 6

    val back: pointer<int> = list.pop() as pointer<int>

    if back == null:
        return 7

    if back.deref != 3:
        return 8

    if list.length != 2:
        return 9

    if (list.peek() as pointer<int>).deref != 2:
        return 10

    val front: pointer<int> = list.popFront() as pointer<int>

    if front == null:
        return 11

    if front.deref != 1:
        return 12

    if list.length != 1:
        return 13

    if (list.peekFront() as pointer<int>).deref != 2:
        return 14

    value.deref = 99
    list.set(0, value)

    if back.deref != 3:
        return 15

    if front.deref != 1:
        return 16

    if (list.peek() as pointer<int>).deref != 99:
        return 17

    val last: pointer<int> = list.pop() as pointer<int>

    if last == null:
        return 18

    if last.deref != 99:
        return 19

    if list.length != 0:
        return 20

    if list.peek() != null:
        return 21

    if list.peekFront() != null:
        return 22

    return 0
}


private fun pushFrontTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
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
    val list: pointer<ArrayList> = new ArrayList(sizeof(int))
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
    val list: pointer<ArrayList> = new ArrayList(sizeof(int), 2, 0.75)
    val valueSpace: blob[sizeof(int)]
    val value: pointer<int> = valueSpace as pointer<int>

    for (var i = 0; i < 6; i++):
    {
        value.deref = i * 3 + 1
        list.push(value)
    }

    if list.length != 6:
        return 1

    for (var i = 0; i < 6; i++):
    {
        val expected: int = i * 3 + 1

        if (list.get(i) as pointer<int>).deref != expected:
            return i + 2
    }

    return 0
}


private fun pointerElementTest() -> int
{
    val list: pointer<ArrayList> = new ArrayList(sizeof(pointer<char>))
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
