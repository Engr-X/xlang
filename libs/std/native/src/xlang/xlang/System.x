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

@file.class("System")
package xlang


private val NS_PER_S: double = 1e-9



@native("xlang_now_s")
native inline fun nowSec() -> long


@native("xlang_now_ms")
native inline fun nowMs() -> long


@native("xlang_now_ns")
native inline fun nowNs() -> long


inline fun now() -> double = nowNs() as double * NS_PER_S


fun memcopy(mut dest: pointer<*>, mut src: pointer<*>, size: int) -> void
{
    if size <= 0: { return; }

    val s: long = src as long
    val d: long = dest as long
    val n: long = size as long
    val longCount: int = size / 8
    val remain: int = size % 8

    if d <= s || d >= s + n
    {
        var srcLong: pointer<long> = src as pointer<long>
        var destLong: pointer<long> = dest as pointer<long>

        for (var i: int = 0; i < longCount; i++, srcLong++, destLong++):
            destLong.deref = srcLong.deref

        var srcByte: pointer<byte> = srcLong as pointer<byte>
        var destByte: pointer<byte> = destLong as pointer<byte>

        for (var i: int = 0; i < remain; i++, srcByte++, destByte++):
            destByte.deref = srcByte.deref
    }
    else
    {
        var srcByteEnd: pointer<byte> = src as pointer<byte> + size
        var destByteEnd: pointer<byte> = dest as pointer<byte> + size

        for (var i: int = 0; i < remain; i++)
        {
            srcByteEnd--
            destByteEnd--
            destByteEnd.deref = srcByteEnd.deref
        }

        var srcLongEnd: pointer<long> = srcByteEnd as pointer<long>
        var destLongEnd: pointer<long> = destByteEnd as pointer<long>

        for (var i: int = 0; i < longCount; i++):
        {
            srcLongEnd--
            destLongEnd--
            destLongEnd.deref = srcLongEnd.deref
        }
    }
}


fun memcopy(dest: pointer<*>, src: pointer<*>, length: int, tsize: int) -> void:
    memcopy(dest, src, length * tsize)


fun memcopy(dest: pointer<*>, src: pointer<*>, srcPos: int, destPos: int, size: int) -> void:
    memcopy(dest as pointer<byte> + destPos, src as pointer<byte> + srcPos, size)
