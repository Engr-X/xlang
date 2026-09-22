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

package xlang.util

import xlang.util.ArrayList
import xlang.util.HashSet


struct MapEntry
{
    var key: pointer<*>

    var value: pointer<*>

    var keyCmp: (pointer<*>, pointer<*>) -> int

    var keyHashCode: (pointer<*>) -> int


    constructor(key: pointer<*>, value: pointer<*>,
        keyCmp: (pointer<*>, pointer<*>) -> int, keyHashCode: (pointer<*>) -> int
    )
    {
        this.key = key
        this.value = value
        this.keyCmp = keyCmp
        this.keyHashCode = keyHashCode
    }
}


private fun hashMapEntryCmp(a: pointer<*>, b: pointer<*>) -> int
{
    val entryA: pointer<MapEntry> =
        a as pointer<MapEntry>

    val entryB: pointer<MapEntry> =
        b as pointer<MapEntry>

    return entryA.keyCmp(
        entryA.key,
        entryB.key
    )
}


private fun hashMapEntryHashCode(item: pointer<*>) -> int
{
    val entry: pointer<MapEntry> = item as pointer<MapEntry>

    return entry.keyHashCode(entry.key)
}


private fun hashMapDefaultHashCode(item: pointer<*>) -> int = 0


struct HashMap
{
    var length: int

    private var entries: pointer<HashSet>

    private var keyCmp: (pointer<*>, pointer<*>) -> int

    private var keyHashCode: (pointer<*>) -> int


    constructor(cmp: (pointer<*>, pointer<*>) -> int)
    {
        this.length = 0
        this.keyCmp = cmp
        this.keyHashCode = hashMapDefaultHashCode

        this.entries = new HashSet(
            sizeof(MapEntry),
            hashMapEntryCmp,
            hashMapEntryHashCode
        )
    }


    constructor(capacity: int, loadFactor: double, cmp: (pointer<*>, pointer<*>) -> int)
    {
        this.length = 0
        this.keyCmp = cmp
        this.keyHashCode = hashMapDefaultHashCode

        this.entries = new HashSet(
            sizeof(MapEntry),
            hashMapEntryCmp,
            hashMapEntryHashCode
        )
    }


    constructor(cmp: (pointer<*>, pointer<*>) -> int, hashCode: (pointer<*>) -> int)
    {
        this.length = 0
        this.keyCmp = cmp
        this.keyHashCode = hashCode

        this.entries = new HashSet(
            sizeof(MapEntry),
            hashMapEntryCmp,
            hashMapEntryHashCode
        )
    }



    private fun initEntry(entry: pointer<MapEntry>, key: pointer<*>, value: pointer<*>)
    {
        entry.key = key
        entry.value = value
        entry.keyCmp = this.keyCmp
        entry.keyHashCode = this.keyHashCode
    }


    private fun findEntry(key: pointer<*>) -> pointer<MapEntry>
    {
        for (var i = 0; i < this.entries.length; i++):
        {
            val entry: pointer<MapEntry> = this.entries.get(i) as pointer<MapEntry>

            if entry != null && this.keyCmp(entry.key, key) == 0:
                return entry
        }

        return null
    }


    fun containsKey(key: pointer<*>) -> bool
    {
        return this.findEntry(key) != null
    }


    fun get(key: pointer<*>) -> pointer<*>
    {
        val entry: pointer<MapEntry> =
            this.findEntry(key)

        if entry == null:
            return null

        return entry.value
    }


    fun put(key: pointer<*>, value: pointer<*>) -> pointer<HashMap>
    {
        val current: pointer<MapEntry> =
            this.findEntry(key)

        if current != null:
        {
            current.value = value
            return this
        }

        val entrySpace: blob[sizeof(MapEntry)]

        val entry: pointer<MapEntry> =
            entrySpace as pointer<MapEntry>

        this.initEntry(
            entry,
            key,
            value
        )

        this.entries.add(entry)
        this.length = this.entries.length

        return this
    }


    fun putIfAbsent(key: pointer<*>, value: pointer<*>) -> bool
    {
        val entrySpace: blob[sizeof(MapEntry)]

        val entry: pointer<MapEntry> =
            entrySpace as pointer<MapEntry>

        this.initEntry(
            entry,
            key,
            value
        )

        val inserted: bool = this.entries.addIfAbsent(entry)

        this.length = this.entries.length

        return inserted
    }


    fun remove(key: pointer<*>) -> bool
    {
        val targetSpace: blob[sizeof(MapEntry)]

        val target: pointer<MapEntry> =
            targetSpace as pointer<MapEntry>

        this.initEntry(target, key, null)

        val removed: bool = this.entries.remove(target)

        this.length = this.entries.length

        return removed
    }


    fun clear()
    {
        this.entries = new HashSet(
            sizeof(MapEntry),
            hashMapEntryCmp,
            hashMapEntryHashCode
        )

        this.length = 0
    }


    fun toArray() -> pointer<ArrayList> = this.entries.toArray()
}
