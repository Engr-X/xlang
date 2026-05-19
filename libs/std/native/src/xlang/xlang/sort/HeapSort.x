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

@file.class("HeapSort")
package xlang.sort

import xlang.System


private fun siftDown(
    src: pointer<*>,
    length: int,
    startIndex: int,
    tsize: int,
    comp: (pointer<*>, pointer<*>) -> int
) -> void
{
    var holeIndex: int = startIndex
    val arr: pointer<byte> = src as pointer<byte>
    val holeValue: blob[tsize]

    System.memcopy(holeValue as pointer<*>, arr + tsize * holeIndex, tsize)

    while holeIndex < length / 2:
    {
        var child: int = holeIndex * 2 + 1
        val right: int = child + 1

        var childPtr: pointer<byte> = arr + tsize * child
        val rightPtr: pointer<byte> = childPtr + tsize

        if right < length && comp(rightPtr, childPtr) > 0:
        {
            child = right
            childPtr = rightPtr
        }

        if comp(holeValue, childPtr) >= 0:
            break

        System.memcopy(arr + tsize * holeIndex, childPtr, tsize)
        holeIndex = child
    }

    System.memcopy(arr + tsize * holeIndex, holeValue, tsize)
}


private fun heapify(src: pointer<*>, length: int, tsize: int, comp: (pointer<*>, pointer<*>) -> int)
{
    if length <= 1:
        return

    var i: int = length / 2

    while i > 0:
    {
        i--
        siftDown(src, length, i, tsize, comp)
    }
}


fun heapSort(src: pointer<*>, length: int, tsize: int, comp: (pointer<*>, pointer<*>) -> int)
{
    if length <= 1:
        return

    heapify(src, length, tsize, comp)

    var heapSize: int = length

    while heapSize > 1:
    {
        heapSize--

        val head: blob[tsize]
        System.memcopy(head as pointer<*>, src, tsize)

        val lastPtr: pointer<byte> = src as pointer<byte> + tsize * heapSize
        System.memcopy(src, lastPtr, tsize)
        System.memcopy(lastPtr, head as pointer<*>, tsize)
        siftDown(src, heapSize, 0, tsize, comp)
    }
}


fun heapSort(
    src: pointer<*>,
    fromIndex: int,
    toIndex: int,
    tsize: int,
    comp: (pointer<*>, pointer<*>) -> int
): heapSort(src as pointer<byte> + tsize * fromIndex, toIndex - fromIndex, tsize, comp)
