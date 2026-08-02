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

@file.class("HashSet")
package xlang.util

import xlang.util.ArrayList


/**
 * Minimal set container backed by ArrayList.
 *
 * This is intentionally a compatibility shell for the future hash-table
 * implementation. It preserves set semantics, but lookup, insertion, and
 * removal are currently linear because every operation scans the backing
 * ArrayList with the configured comparator.
 *
 * The comparator contract is the same as ArrayList:
 * - first argument is the stored element slot
 * - second argument is the caller-provided item
 * - return 0 when the two values are equal
 *
 * The set stores byte copies of fixed-width element slots. For pointer<T>
 * elements, construct it with sizeof(pointer<T>) and pass pointer slot
 * addresses to add(), contains(), and remove().
 */
struct HashSet
{
    var length: int

    private var list: pointer<ArrayList>
    private var cmp: (pointer<*>, pointer<*>) -> int


    /**
     * Creates an empty set for fixed-width elements.
     *
     * @param tsize             size in bytes of one stored element slot
     * @param cmp               equality comparator
     */
    fun __init__(tsize: int, cmp: (pointer<*>, pointer<*>) -> int)
    {
        this.length = 0
        this.cmp = cmp
        this.list = new ArrayList(tsize)
        this.list.setCmparator(cmp)
    }


    /**
     * Creates an empty set with explicit ArrayList allocation settings.
     *
     * @param tsize             size in bytes of one stored element slot
     * @param initialCapacity   initial backing-list capacity
     * @param loadFactor        backing-list resize threshold
     * @param cmp               equality comparator
     */
    fun __init__(tsize: int, initialCapacity: int, loadFactor: double, cmp: (pointer<*>, pointer<*>) -> int)
    {
        this.length = 0
        this.cmp = cmp
        this.list = new ArrayList(tsize, initialCapacity, loadFactor, cmp)
    }


    /**
     * Returns true if an equal element is already present.
     */
    fun contains(item: pointer<*>) -> bool =
        this.list.contains(item)


    /**
     * Adds item if it is not already present.
     *
     * The item bytes are copied into the set. Adding a duplicate leaves the set
     * unchanged.
     *
     * @return                  true when a new element was inserted
     */
    fun addIfAbsent(item: pointer<*>) -> bool
    {
        if this.contains(item):
            return false

        this.list.push(item)
        this.length = this.list.length
        return true
    }


    /**
     * Adds item and returns this set for chained initialization.
     *
     * This has the same insertion semantics as addIfAbsent(), but discards the
     * inserted/duplicate result so callers can write:
     *     set.add(a).add(b).add(c)
     */
    fun add(item: pointer<*>) -> pointer<HashSet>
    {
        this.addIfAbsent(item)
        return this
    }


    /**
     * Removes item if an equal element exists.
     *
     * @return                  true when an element was removed
     */
    fun remove(item: pointer<*>) -> bool
    {
        val index: int = this.list.indexOf(item)

        if index < 0:
            return false

        this.list.removeAt(index)
        this.length = this.list.length
        return true
    }


    /**
     * Returns the stored element slot at index.
     *
     * The returned pointer belongs to the backing ArrayList and is invalidated
     * by later mutations that resize or shift the list.
     */
    fun get(index: int) -> pointer<*> =
        this.list.get(index)


    /**
     * Returns the backing storage as an ArrayList clone.
     */
    fun toArray() -> pointer<ArrayList> =
        this.list.clone()
}
