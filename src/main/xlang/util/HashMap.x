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

package xlang.util

import xlang.util.ArrayList
import xlang.util.HashSet


struct HashMapEntry
{
    var key: pointer<*>

    var value: pointer<*>

    var keyCmp: (pointer<*>, pointer<*>) -> int


    constructor(
        key: pointer<*>,
        value: pointer<*>,
        keyCmp: (pointer<*>, pointer<*>) -> int
    )
    {
        this.key = key
        this.value = value
        this.keyCmp = keyCmp
    }
}


fun hashMapEntryCmp(a: pointer<*>, b: pointer<*>) -> int
{
    val entryA: pointer<HashMapEntry> = a
    val entryB: pointer<HashMapEntry> = b

    return entryA.keyCmp(entryA.key, entryB.key)
}


struct HashMap
{
    var length: int

    private var entries: pointer<HashSet>

    private var keyCmp: (pointer<*>, pointer<*>) -> int


    constructor(cmp: (pointer<*>, pointer<*>) -> int)
    {
        this.length = 0
        this.keyCmp = cmp
        this.entries = new HashSet(
            sizeof(HashMapEntry),
            hashMapEntryCmp
        )
    }


    constructor(
        initialCapacity: int,
        loadFactor: double,
        cmp: (pointer<*>, pointer<*>) -> int
    )
    {
        this.length = 0
        this.keyCmp = cmp
        this.entries = new HashSet(
            sizeof(HashMapEntry),
            initialCapacity,
            loadFactor,
            hashMapEntryCmp
        )
    }


    private fun makeEntry(
        key: pointer<*>,
        value: pointer<*>
    ) -> HashMapEntry =
        HashMapEntry(key, value, this.keyCmp)


    private fun indexOf(key: pointer<*>) -> int
    {
        val targetSpace: blob[sizeof(HashMapEntry)]
        val target: pointer<HashMapEntry> =
            targetSpace as pointer<HashMapEntry>

        target.key = key
        target.value = null
        target.keyCmp = this.keyCmp

        var i: int = 0

        while i < this.entries.length:
        {
            val entry: pointer<HashMapEntry> =
                this.entries.get(i)

            if hashMapEntryCmp(entry, target) == 0:
                return i

            i++
        }

        return -1
    }


    fun containsKey(key: pointer<*>) -> bool =
        this.indexOf(key) >= 0


    fun get(key: pointer<*>) -> pointer<*>
    {
        val index: int = this.indexOf(key)

        if index < 0:
            return null

        val entry: pointer<HashMapEntry> =
            this.entries.get(index)

        return entry.value
    }


    fun put(
        key: pointer<*>,
        value: pointer<*>
    ) -> pointer<HashMap>
    {
        val index: int = this.indexOf(key)

        if index >= 0:
        {
            val entry: pointer<HashMapEntry> =
                this.entries.get(index)

            entry.value = value
            return this
        }

        val entrySpace: blob[sizeof(HashMapEntry)]
        val entry: pointer<HashMapEntry> =
            entrySpace as pointer<HashMapEntry>

        entry.key = key
        entry.value = value
        entry.keyCmp = this.keyCmp

        this.entries.add(entry)
        this.length = this.entries.length

        return this
    }


    fun putIfAbsent(
        key: pointer<*>,
        value: pointer<*>
    ) -> bool
    {
        if this.containsKey(key):
            return false

        val entrySpace: blob[sizeof(HashMapEntry)]
        val entry: pointer<HashMapEntry> =
            entrySpace as pointer<HashMapEntry>

        entry.key = key
        entry.value = value
        entry.keyCmp = this.keyCmp

        this.entries.add(entry)
        this.length = this.entries.length

        return true
    }


    fun remove(key: pointer<*>) -> bool
    {
        val index: int = this.indexOf(key)

        if index < 0:
            return false

        val entry: pointer<HashMapEntry> =
            this.entries.get(index)

        val removed: bool =
            this.entries.remove(entry)

        this.length = this.entries.length

        return removed
    }


    fun clear()
    {
        while this.entries.length > 0:
        {
            val entry: pointer<*> =
                this.entries.get(this.entries.length - 1)

            this.entries.remove(entry)
        }

        this.length = 0
    }


    fun toArray() -> pointer<ArrayList> =
        this.entries.toArray()
}
