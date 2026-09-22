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


struct HashSet
{
    private static val DEFAULT_CAPACITY: int = 16

    private static val DEFAULT_LOAD_FACTOR: double = 0.75

    var length: int

    private var tsize: int

    private var bucketCount: int

    private var loadFactor: double

    private var buckets: pointer<ArrayList>

    private var cmp: (pointer<*>, pointer<*>) -> int

    private var hashCode: (pointer<*>) -> int


    constructor(tsize: int, bucketCount: int, loadFactor: double, cmp: (pointer<*>, pointer<*>) -> int, hashCode: (pointer<*>) -> int)
    {
        this.length = 0
        this.tsize = tsize
        this.bucketCount = bucketCount
        this.loadFactor = loadFactor
        this.cmp = cmp
        this.hashCode = hashCode
        this.buckets = this.createBuckets(this.bucketCount)
    }


    constructor(tsize: int, cmp: (pointer<*>, pointer<*>) -> int, hashCode: (pointer<*>) -> int)
    {
        this.length = 0
        this.tsize = tsize
        this.bucketCount = DEFAULT_CAPACITY
        this.loadFactor = DEFAULT_LOAD_FACTOR
        this.cmp = cmp
        this.hashCode = hashCode
        this.buckets = this.createBuckets(this.bucketCount)
    }


    private fun createBuckets(count: int) -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(pointer<ArrayList>))

        for (var i: int = 0; i < count; i++):
        {
            val bucket: pointer<ArrayList> = new ArrayList(this.tsize)

            bucket.setComparator(this.cmp)
            result.push(bucket.ref)
        }

        return result
    }


    private fun getBucket(index: int) -> pointer<ArrayList>
    {
        val slot: pointer<pointer<ArrayList>> =
            this.buckets.get(index) as pointer<pointer<ArrayList>>

        if slot == null:
            return null

        return slot.deref
    }


    private fun getBucketIndex(item: pointer<*>) -> int
    {
        var index: int = this.hashCode(item) % this.bucketCount

        if index < 0:
            index += this.bucketCount

        return index
    }


    private fun getBucketFor(item: pointer<*>) -> pointer<ArrayList> =
        this.getBucket(this.getBucketIndex(item))


    fun contains(item: pointer<*>) -> bool
    {
        if item == null:
            return false

        val bucket: pointer<ArrayList> =
            this.getBucketFor(item)

        if bucket == null:
            return false

        return bucket.contains(item)
    }


    fun addIfAbsent(item: pointer<*>) -> bool
    {
        if item == null:
            return false

        var bucket: pointer<ArrayList> =
            this.getBucketFor(item)

        if bucket != null && bucket.contains(item):
            return false

        if ((this.length + 1) as double) > (this.bucketCount as double) * this.loadFactor:
        {
            this.rehash(this.bucketCount * 2)
            bucket = this.getBucketFor(item)
        }

        if bucket == null:
            return false

        bucket.push(item)
        this.length++
        return true
    }


    fun add(item: pointer<*>) -> pointer<HashSet>
    {
        this.addIfAbsent(item)
        return this
    }


    fun remove(item: pointer<*>) -> bool
    {
        if item == null:
            return false

        val bucket: pointer<ArrayList> =
            this.getBucketFor(item)

        if bucket == null:
            return false

        val index: int =
            bucket.indexOf(item)

        if index < 0:
            return false

        bucket.removeAt(index)
        this.length--
        return true
    }


    private fun rehash(newBucketCount: int)
    {
        val oldBuckets: pointer<ArrayList> =
            this.buckets

        val oldBucketCount: int =
            this.bucketCount

        this.bucketCount = newBucketCount
        this.buckets = this.createBuckets(newBucketCount)

        for (var i: int = 0; i < oldBucketCount; i++):
        {
            val slot: pointer<pointer<ArrayList>> =
                oldBuckets.get(i) as pointer<pointer<ArrayList>>

            if slot == null:
                continue

            val oldBucket: pointer<ArrayList> =
                slot.deref

            if oldBucket == null:
                continue

            for (var j: int = 0; j < oldBucket.length; j++):
            {
                val item: pointer<*> =
                    oldBucket.get(j)

                if item == null:
                    continue

                val bucket: pointer<ArrayList> =
                    this.getBucketFor(item)

                if bucket != null:
                    bucket.push(item)
            }
        }
    }


    fun get(index: int) -> pointer<*>
    {
        if index < 0 || index >= this.length:
            return null

        var offset: int = 0

        for (var i: int = 0; i < this.bucketCount; i++):
        {
            val bucket: pointer<ArrayList> =
                this.getBucket(i)

            if bucket == null:
                continue

            if index < offset + bucket.length:
                return bucket.get(index - offset)

            offset += bucket.length
        }

        return null
    }


    fun toArray() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> =
            new ArrayList(this.tsize)

        result.setComparator(this.cmp)

        for (var i: int = 0; i < this.bucketCount; i++):
        {
            val bucket: pointer<ArrayList> =
                this.getBucket(i)

            if bucket != null:
                result.pushAll(bucket)
        }

        return result
    }
}
