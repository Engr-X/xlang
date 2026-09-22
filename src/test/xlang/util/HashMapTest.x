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
 */

#file.outerClass("HashMapTest")
package xlang.util

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.util.HashMap")
    val putGetTC: pointer<TestCase> = new TestCase("putGet", putGetTest)
    val putIfAbsentTC: pointer<TestCase> = new TestCase("putIfAbsent", putIfAbsentTest)
    val removeClearTC: pointer<TestCase> = new TestCase("removeClear", removeClearTest)
    val toArrayTC: pointer<TestCase> = new TestCase("toArray", toArrayTest)

    result.addTestUnion(new TestUnion(TestCase.TYPE, putGetTC, null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, putIfAbsentTC, null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, removeClearTC, null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, toArrayTC, null))

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


private fun putGetTest() -> int
{
    val map: pointer<HashMap> = new HashMap(intCmp)
    val keySpace: blob[sizeof(int) * 3]
    val valueSpace: blob[sizeof(int) * 3]
    val keys: pointer<int> = keySpace as pointer<int>
    val values: pointer<int> = valueSpace as pointer<int>

    keys[0] = 1
    keys[1] = 2
    keys[2] = 3
    values[0] = 10
    values[1] = 20
    values[2] = 30

    if map.containsKey(keys):
        return 1

    if map.get(keys) != null:
        return 2

    map.put(keys, values)

    if map.length != 1:
        return 3

    if !map.containsKey(keys):
        return 4

    val firstValue: pointer<int> = map.get(keys) as pointer<int>

    if firstValue == null || firstValue.deref != 10:
        return 5

    map.put(keys + 1, values + 1)

    if map.length != 2:
        return 6

    val secondValue: pointer<int> = map.get(keys + 1) as pointer<int>

    if secondValue == null || secondValue.deref != 20:
        return 7

    map.put(keys, values + 2)

    if map.length != 2:
        return 8

    val updatedValue: pointer<int> = map.get(keys) as pointer<int>

    if updatedValue == null || updatedValue.deref != 30:
        return 9

    if map.get(keys + 2) != null:
        return 10

    return 0
}


private fun putIfAbsentTest() -> int
{
    val map: pointer<HashMap> = new HashMap(intCmp)
    val keySpace: blob[sizeof(int) * 2]
    val valueSpace: blob[sizeof(int) * 2]
    val keys: pointer<int> = keySpace as pointer<int>
    val values: pointer<int> = valueSpace as pointer<int>

    keys[0] = 4
    keys[1] = 5
    values[0] = 40
    values[1] = 50

    if !map.putIfAbsent(keys, values):
        return 1

    if map.length != 1:
        return 2

    if map.putIfAbsent(keys, values + 1):
        return 3

    if map.length != 1:
        return 4

    val firstValue: pointer<int> = map.get(keys) as pointer<int>

    if firstValue == null || firstValue.deref != 40:
        return 5

    if !map.putIfAbsent(keys + 1, values + 1):
        return 6

    if map.length != 2:
        return 7

    return 0
}


private fun removeClearTest() -> int
{
    val map: pointer<HashMap> = new HashMap(2, 0.75, intCmp)
    val keySpace: blob[sizeof(int) * 4]
    val valueSpace: blob[sizeof(int) * 3]
    val keys: pointer<int> = keySpace as pointer<int>
    val values: pointer<int> = valueSpace as pointer<int>

    keys[0] = 6
    keys[1] = 7
    keys[2] = 8
    keys[3] = 9
    values[0] = 60
    values[1] = 70
    values[2] = 80

    map.put(keys, values)
    map.put(keys + 1, values + 1)
    map.put(keys + 2, values + 2)

    if map.length != 3:
        return 1

    if map.remove(keys + 3):
        return 2

    if !map.remove(keys + 1):
        return 3

    if map.length != 2:
        return 4

    if map.containsKey(keys + 1):
        return 5

    if map.get(keys + 1) != null:
        return 6

    if !map.containsKey(keys) || !map.containsKey(keys + 2):
        return 7

    map.clear()

    if map.length != 0:
        return 8

    if map.containsKey(keys) || map.containsKey(keys + 2):
        return 9

    return 0
}


private fun toArrayTest() -> int
{
    val map: pointer<HashMap> = new HashMap(intCmp)
    val keySpace: blob[sizeof(int) * 3]
    val valueSpace: blob[sizeof(int) * 3]
    val keys: pointer<int> = keySpace as pointer<int>
    val values: pointer<int> = valueSpace as pointer<int>

    keys[0] = 10
    keys[1] = 11
    keys[2] = 12
    values[0] = 100
    values[1] = 110
    values[2] = 120

    map.put(keys, values)
    map.put(keys + 1, values + 1)

    val array: pointer<ArrayList> = map.toArray()

    if array == null:
        return 1

    if array.length != 2:
        return 2

    val firstEntry: pointer<HashMapEntry> = array.get(0) as pointer<HashMapEntry>
    val secondEntry: pointer<HashMapEntry> = array.get(1) as pointer<HashMapEntry>

    if firstEntry == null || secondEntry == null:
        return 3

    if (firstEntry.key as pointer<int>).deref != 10:
        return 4

    if (firstEntry.value as pointer<int>).deref != 100:
        return 5

    if (secondEntry.key as pointer<int>).deref != 11:
        return 6

    if (secondEntry.value as pointer<int>).deref != 110:
        return 7

    map.put(keys + 2, values + 2)

    if array.length != 2:
        return 8

    return 0
}
