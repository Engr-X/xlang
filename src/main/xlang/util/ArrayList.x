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
 */

@file.class("ArrayList")
package xlang.util

import xlang.System


/**
 * Dynamically sized array for fixed-width elements.
 *
 * This type is intentionally untyped at runtime. It stores raw bytes in a
 * contiguous buffer, and every element is assumed to have exactly the same byte
 * width. The caller provides that width through tsize when constructing the
 * list.
 *
 * Memory model:
 * - data owns capacity * tsize bytes of heap storage.
 * - length is the number of initialized element slots.
 * - valid element bytes are stored in [data, data + length * tsize).
 * - ArrayList only copies element bytes; it does not know how to destroy,
 *   clone, or free nested data referenced by those bytes.
 *
 * Pointer element usage:
 * If the element type is pointer<T>, construct the list with
 * sizeof(pointer<T>). push(), pushFront(), add(), and set() expect the address
 * of the pointer slot, not the pointer value itself. addAll() expects src to
 * point to a contiguous range of pointer slots. get() returns the address of
 * the stored slot inside the list.
 *
 * Example:
 *     var value: pointer<T> = ...
 *     list.push(value.ref)
 *     val slot: pointer<pointer<T>> = list.get(0) as pointer<pointer<T>>
 *     val copiedValue: pointer<T> = slot.deref
 *
 * Pointers returned by get() point into the internal buffer. They become
 * invalid after any operation that reallocates the buffer.
 */
struct ArrayList
{
    /*
     * Default number of element slots allocated by the single-argument constructor.
     */
    private static val LIST_INITIAL_CAPACITY: int = 10


    /*
     * Default resize threshold used by the single-argument constructor.
     *
     * When the next insertion would make length reach capacity * loadFactor,
     * the backing buffer is grown before the insertion is written.
     */
    private static val LIST_LOAD_FACTOR: double = 0.75


    /*
     * Number of initialized elements currently stored in the list.
     *
     * Invariant: 0 <= length && length <= capacity.
     */
    var length: int


    /*
     * Size in bytes of exactly one element slot.
     *
     * This value is immutable after construction. Callers are responsible for
     * passing a positive value that matches the element type they store.
     */
    private val tsize: int


    /*
     * Raw backing storage.
     *
     * Slot i begins at data + i * tsize. Slots from 0 up to length - 1 contain
     * initialized values. Slots from length up to capacity - 1 are allocated but
     * should be treated as uninitialized.
     */
    private var data: pointer<byte>


    /*
     * Per-list resize threshold.
     *
     * This allows callers to trade memory overhead for insertion frequency when
     * using the explicit constructor.
     */
    private var loadFactor: double


    /*
     * Number of element slots currently allocated in data.
     *
     * The allocated byte size is capacity * tsize.
     */
    private var capacity: int


    /**
     * Creates an empty list using the default capacity and load factor.
     *
     * Preconditions:
     * - tsize must be greater than 0.
     *
     * Postconditions:
     * - length == 0.
     * - capacity == LIST_INITIAL_CAPACITY.
     * - data points to capacity * tsize bytes of storage.
     *
     * @param tsize             size in bytes of one element
     */
    fun __init__(tsize: int)
    {
        this.length = 0
        this.tsize = tsize
        this.capacity = LIST_INITIAL_CAPACITY
        this.loadFactor = LIST_LOAD_FACTOR
        this.data = System.allocMemory(tsize * this.capacity) as pointer<byte>
    }


    /**
     * Creates an empty list with explicit allocation settings.
     *
     * This constructor is useful when the expected list size is known, or when
     * the caller wants a different growth threshold. The values are not
     * normalized by this type, so invalid values are caller errors.
     *
     * Preconditions:
     * - tsize must be greater than 0.
     * - initialCapacity must be greater than 0.
     * - loadFactor must be greater than 0.
     *
     * Postconditions:
     * - length == 0.
     * - capacity == initialCapacity.
     * - data points to initialCapacity * tsize bytes of storage.
     *
     * @param tsize            size in bytes of one element
     * @param initialCapacity  number of element slots to allocate initially
     * @param loadFactor       resize threshold as a ratio of capacity
     */
    fun __init__(tsize: int, initialCapacity: int, loadFactor: double)
    {
        this.length = 0
        this.tsize = tsize
        this.capacity = initialCapacity
        this.loadFactor = loadFactor
        this.data = System.allocMemory(tsize * this.capacity) as pointer<byte>
    }


    /**
     * Ensures enough backing storage for an upcoming insertion.
     *
     * The required capacity is:
     * current length + inserted element count + one spare slot.
     *
     * If the current capacity is too small, it is doubled until it can hold the
     * required number of element slots. System.reallocMemory may move the
     * buffer, so any pointer previously returned by get() must be considered
     * invalid after this function runs.
     *
     * @param insertLength      number of elements that will be inserted
     */
    private inline fun resize(insertLength: int)
    {
        val requiredCapacity: int = insertLength + this.length + 1

        while this.capacity < requiredCapacity:
            this.capacity *= 2

        this.data = System.reallocMemory(this.data, this.capacity * this.tsize) as pointer<byte>
    }


    /**
     * Copies tsize bytes from item into the slot at index.
     *
     * Preconditions:
     * - index must be a valid element index (0 <= index < length).
     * - item must point to at least tsize readable bytes.
     *
     * Postconditions:
     * - The slot at index contains a byte copy of item.
     *
     * @param index             index of the slot to copy into
     * @param item              pointer to the source bytes to copy
     */
    private inline fun copyToSlot(index: int, item: pointer<*>):
        System.memcopy(this.data + index * this.tsize, item, this.tsize)


    /**
     * Moves a contiguous range of initialized slots inside the backing buffer.
     *
     * Slot indexes are converted to byte offsets using tsize. length is counted
     * in element slots, not bytes. The source and destination ranges may
     * overlap; this relies on System.memcopy being the runtime primitive used
     * throughout ArrayList for overlapping internal shifts.
     *
     * @param destIndex         first destination slot
     * @param fromIndex         first source slot
     * @param length            number of slots to move
     */
    private inline fun moveSlots(destIndex: int, fromIndex: int, length: int):
        System.memcopy(
            this.data + destIndex * this.tsize,
            this.data + fromIndex * this.tsize,
            length * this.tsize)



    /**
     * Checks whether an index is inside the valid element range.
     *
     * When inclusive is false, valid indexes are existing elements:
     *     0 <= index && index < length
     *
     * When inclusive is true, index == length is also accepted. That form is for
     * append/insert-style operations where the position directly after the last
     * element is meaningful.
     */
    private inline fun checkIndex(index: int, inclusive: bool) -> bool
    {
        if index < 0:
            return false

        return if inclusive:
            index <= this.length
        else:
            index < this.length
    }

    
    /**
     * Appends one element by copying tsize bytes from item.
     *
     * The source pointer is only used during the copy. After push() returns, the
     * stored element is independent from the source storage.
     *
     * Preconditions:
     * - item must point to at least tsize readable bytes.
     * - item should not point into this list's internal buffer if this push may
     *   trigger resize(), because resize() can move that buffer before the copy.
     *
     * Postconditions:
     * - The previous length becomes the index of the inserted element.
     * - length is increased by 1.
     */
    fun push(item: pointer<*>)
    {
        if this.length + 1 >= ((this.capacity as double) * this.loadFactor) as int:
            this.resize(1)

        this.copyToSlot(this.length, item)
        this.length++
    }


    /**
     * Inserts one element at the front of the list.
     *
     * Existing elements are shifted right by one slot before item is copied into
     * index 0. This operation is O(length).
     *
     * Existing bytes are moved from the end of the initialized range toward the
     * front, so the source bytes are not overwritten before they are copied.
     *
     * Preconditions:
     * - item must point to at least tsize readable bytes.
     * - item should not point into this list's internal buffer. The shift changes
     *   the contents of that buffer before item is copied into index 0.
     *
     * Postconditions:
     * - The inserted element is stored at index 0.
     * - All previous elements move to index + 1.
     * - length is increased by 1.
     */
    fun pushFront(item: pointer<*>)
    {
        if this.length + 1 >= ((this.capacity as double) * this.loadFactor) as int:
            this.resize(1)

        this.moveSlots(1, 0, this.length)
        this.copyToSlot(0, item)
        this.length++
    }


    /**
     * Inserts a contiguous range of elements before index.
     *
     * length is counted in element slots, not bytes. The copied byte count is
     * length * tsize. The source pointer is only used during the copy; after
     * addAll() returns, stored elements are independent from the source storage.
     *
     * For pointer<T> elements, src must point to a contiguous range of
     * pointer<T> slots, not to the first pointed-to object.
     *
     * index may point to an existing element, or it may be exactly length. When
     * index == length, this function appends the range. Invalid indexes and
     * non-positive lengths are ignored.
     *
     * Preconditions:
     * - src must point to at least length * tsize readable bytes when length > 0.
     * - src should not point into this list's internal buffer if this call may
     *   trigger resize(), because resize() can move that buffer before copying.
     *
     * Postconditions:
     * - If index is valid and length > 0, src is copied into slot index.
     * - Existing elements at index and after it move right by length slots.
     * - this.length is increased by length.
     */
    fun addAll(index: int, items: pointer<*>, length: int)
    {
        if !this.checkIndex(index, true):
            return

        if length <= 0:
            return

        if this.length + length >= ((this.capacity as double) * this.loadFactor) as int:
            this.resize(length)

        this.moveSlots(index + length, index, this.length - index)
        System.memcopy(this.data + index * this.tsize, items, length * this.tsize)
        this.length += length
    }


    /**
     * Inserts one element before index.
     *
     * index may point to an existing element, or it may be exactly length. When
     * index == length, this function behaves like push(). Invalid indexes are
     * ignored.
     *
     * Existing elements at index and after it are shifted right by one slot.
     * This operation is O(length - index).
     *
     * Preconditions:
     * - item must point to at least tsize readable bytes.
     * - item should not point into this list's internal buffer. The shift can
     *   move or overwrite internal slots before item is copied into index.
     *
     * Postconditions:
     * - If index is valid, item is copied into slot index.
     * - If index is valid, length is increased by 1.
     * - If index is invalid, length and contents are unchanged.
     */
    fun add(index: int, item: pointer<*>)
    {
        if !this.checkIndex(index, true):
            return

        if this.length + 1 >= ((this.capacity as double) * this.loadFactor) as int:
            this.resize(1)

        this.moveSlots(index + 1, index, this.length - index)

        this.copyToSlot(index, item)
        this.length++
    }


    /**
     * Removes the element at index.
     *
     * Elements after index are shifted left by one slot. The bytes in the old
     * last slot are left unspecified and should be treated as uninitialized
     * after length is decreased. Invalid indexes are ignored.
     *
     * This operation only removes the stored bytes from the list. If the stored
     * element is a pointer, the pointed-to object is not freed.
     *
     * Postconditions:
     * - If index is valid, length is decreased by 1.
     * - If index is invalid, length and contents are unchanged.
     */
    fun removeAt(index: int)
    {
        if !this.checkIndex(index, false):
            return

        this.moveSlots(index, index + 1, this.length - index - 1)

        this.length--
    }


    /**
     * Removes the first element that compares equal to item.
     *
     * Equality is decided by cmp(slot, item) == 0. slot points to the stored
     * element bytes inside this list; item is the caller-provided search value.
     * This means pointer-element callers should pass the address of a pointer
     * slot, just like push() and set().
     *
     * If no element matches, the list is unchanged.
     */
    fun remove(item: pointer<*>, cmp: (pointer<*>, pointer<*>) -> int)
    {
        val index: int = this.indexOf(item, cmp)
        
        if index >= 0:
            this.removeAt(index)
    }


    /**
     * Returns the index of the first element that compares equal to item.
     *
     * cmp is called as cmp(slot, item), where slot is the address of the stored
     * element slot and item is the caller-provided search value. A cmp result of
     * 0 means equal.
     *
     * @return first matching index, or -1 when no element matches
     */
    fun indexOf(item: pointer<*>, cmp: (pointer<*>, pointer<*>) -> int) -> int
    {
        for (var i = 0; i < this.length; i++):
        {
            val slot: pointer<byte> = this.data + i * this.tsize
            if cmp(slot, item) == 0:
                return i
        }

        return -1
    }


    /**
     * Replaces the element at index by copying tsize bytes from item.
     *
     * Invalid indexes are ignored instead of producing an error. This keeps the
     * container small and avoids adding error handling machinery before the
     * language has a richer standard-library convention for it.
     *
     * Preconditions:
     * - item must point to at least tsize readable bytes.
     *
     * Postconditions:
     * - length is unchanged.
     * - If index is valid, slot index contains a byte copy of item.
     */
    fun set(index: int, item: pointer<*>)
    {
        if !this.checkIndex(index, false):
            return

        this.copyToSlot(index, item)
    }


    /**
     * Returns the address of the stored element slot at index.
     *
     * The return value is a pointer into the internal buffer. Cast it to the
     * element slot pointer type before reading or writing through it.
     *
     * Example for int elements:
     *     val slot: pointer<int> = list.get(i) as pointer<int>
     *     val value: int = slot.deref
     *
     * Example for pointer<T> elements:
     *     val slot: pointer<pointer<T>> = list.get(i) as pointer<pointer<T>>
     *     val value: pointer<T> = slot.deref
     *
     * Invalid indexes return null. A non-null returned pointer remains valid only
     * until the next resize of this list.
     */
    fun get(index: int) -> pointer<*>
    {
        if !this.checkIndex(index, false):
            return null

        return this.data + index * this.tsize
    }
}
