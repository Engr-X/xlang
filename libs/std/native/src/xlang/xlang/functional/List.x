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

@file.class("List")
package xlang.functional

import xlang.System



// fun concat(src1: pointer<*>, src2: pointer<*>, dest: pointer<*>, tsize: int) -> void
// {
//     System.memcopy
// }


// fun tail(dest: pointer<*>, src: pointer<*>, length: int, tsize: int) -> void
// {
    
// }


fun reverse(src: pointer<*>, length: int, tsize: int) -> void
{
    if length <= 1:
        return

    val buffer: blob[tsize]

    for (
        var ptrI: pointer<byte> = src as pointer<byte>,
            ptrJ: pointer<byte> = src as pointer<byte> + (length - 1) * tsize;
        ptrI < ptrJ;
        ptrI += tsize, ptrJ -= tsize
    ):
    {
        System.memcopy(buffer as pointer<*>, ptrI, tsize)
        System.memcopy(ptrI, ptrJ, tsize)
        System.memcopy(ptrJ, buffer as pointer<*>, tsize)
    }
}


// PRE: length is positive!
fun foldl<T, U>(src: pointer<T>, length: int, init: U, f: (U, T) -> U) -> U
{
    var acc: U = init

    for (var i = 0; i < length; i++):
        acc = f(acc, src[i])

    return acc
}


// PRE: length is positive!
fun foldr<T, U>(src: pointer<T>, length: int, init: U, f: (T, U) -> U) -> U
{
    var acc: U = init

    for (var i = length - 1; i >= 0; i--):
        acc = f(src[i], acc)

    return acc
}


fun conjunct<T, U>(src: pointer<T>, length: int, init: U, f: (T, U) -> U) -> U
{
    var acc: U = init

    for (var i = length - 1; i >= 0; i--):
        acc = f(src[i], acc)

    return acc
}


fun disjunct(src: pointer<bool>, length: int) -> bool
{
    for (var i = 0; i < length; i++):
    {
        if src[i]:
            return true
    }

    return false
}


fun any(f: (item: pointer<*>) -> bool, src: pointer<*>, length: int, tsize: int) -> bool
{
    for (
        var i = 0, ptr: pointer<byte> = src as pointer<byte>;
        i < length;
        i++, ptr += tsize
    ):
    {
        if f(ptr):
            return true
    }

    return false
}


fun all(f: (item: pointer<*>) -> bool, src: pointer<*>, length: int, tsize: int) -> bool
{
    for (
        var i = 0, ptr: pointer<byte> = src as pointer<byte>;
        i < length;
        i++, ptr += tsize
    ):
    {
        if !f(ptr):
            return false
    }

    return true
}


// TODO T is the subclass for num
fun sum<T>(src: pointer<T>, length: int) -> T
{
    if length <= 0:
        return 0 as T

    var sum: T = src[0]

    for (var i = 0; i < length; i++, sum += src[i]);

    return sum
}


fun product<T>(src: pointer<T>, length: int) -> T
{
    if length <= 0:
        return 1 as T

    var sum: T = src[0]

    for (var i = 0; i < length; i++):
    {
        if src[i] == (0 as T):
            return (0 as T)

        sum *= src[i]
    }

    return sum
}


// pre length >= 1
fun maximum<T>(src: pointer<T>, length: int) -> T
{
    var maxItem: T = src[0]

    for (var i: int = 0; i < length; i++):
    {
        if src[i] > maxItem:
            maxItem = src[i]
    }

    return maxItem
}


// pre length >= 1
fun minimum<T>(src: pointer<T>, length: int) -> T
{
    var minItem: T = src[0]

    for (var i: int = 0; i < length; i++):
    {
        if src[i] < minItem:
            minItem = src[i]
    }

    return minItem
}
