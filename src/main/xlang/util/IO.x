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

@file.class("IO")
package xlang.util

import xlang.System


/**
 * Native I/O utility bindings.
 *
 * This module declares low-level native functions used by the XLang runtime.
 * The functions are implemented outside XLang and are linked through their
 * native symbol names.
 *
 * The colored output helpers write formatted ANSI-colored strings into a
 * caller-provided destination buffer. The caller must ensure that the buffer
 * has enough capacity for the generated text and the final null terminator.
 */


/**
 * Gets the byte size of a file.
 *
 * This is a low-level native binding used by higher-level file helpers.
 * It reads the file size in binary mode so the result matches the number of
 * bytes that readFileToBuffer expects to copy.
 *
 * @param path                  pointer to the null-terminated file path
 * @return                      file size in bytes, or a negative native error code
 */
@native("filesize")
private native inline fun filesize(path: pointer<char>) -> int;


/**
 * Reads a whole file into a caller-provided buffer.
 *
 * This is a low-level native binding used by higher-level file helpers.
 * The destination buffer must have at least filesize(path) + 1 bytes because
 * the native implementation appends a final null terminator after the bytes
 * read from disk.
 *
 * @param dest                  pointer to the destination character buffer
 * @param path                  pointer to the null-terminated file path
 * @return                      number of bytes read, or a negative native error code
 */
@native("read")
private native inline fun readFileToBuffer(dest: pointer<char>, path: pointer<char>) -> int;


/**
 * Reads a whole file into a newly allocated null-terminated buffer.
 *
 * The file is read as raw bytes by the native implementation, so line endings
 * are preserved exactly as they exist on disk. The returned buffer is allocated
 * with one extra byte for the final null terminator, which makes it usable as a
 * pointer<char> source buffer for tokenizer-style code.
 *
 * Empty files, invalid paths, and allocation failures return null.
 *
 * @param path                  pointer to the null-terminated file path
 * @return                      newly allocated file content buffer, or null on failure
 */
fun readFile(path: pointer<char>) -> pointer<char>
{
    val size: int = filesize(path)

    if size <= 0:
        return null

    val buffer: pointer<char> = System.allocMemory(size + 1) as pointer<char>

    if buffer == null:
        return null

    readFileToBuffer(buffer, path)
    return buffer
}


/**
 * Enables ANSI color support for the current output environment.
 *
 * On platforms where ANSI color is already enabled, this function may simply
 * return success without changing anything. On platforms that require explicit
 * setup, the native runtime performs the required initialization.
 *
 * @return native status code, usually 0 on success
 */
@native("enable_ansi_color")
native inline fun enableANSIColor() -> int;


/**
 * Writes a colored string into a destination buffer.
 *
 * The generated string does not automatically add a line feed.
 * The destination buffer must have enough capacity to store the colored output,
 * including any ANSI escape sequences inserted by the native runtime and the
 * final null terminator.
 *
 * @param dest                  pointer to the destination character buffer
 * @param value                 pointer to the null-terminated string to format
 * @param color                 color code used by the native runtime
 * @return                      number of characters written
 */
@native("colored_sprint")
native inline fun coloredSprint(dest: pointer<char>, value: pointer<char>, color: int) -> int;


/**
 * Writes a colored string followed by a line feed into a destination buffer.
 *
 * The destination buffer must have enough capacity to store the colored output,
 * the line feed, any ANSI escape sequences inserted by the native runtime, and
 * the final null terminator.
 *
 * @param dest                  pointer to the destination character buffer
 * @param value                 pointer to the null-terminated string to format
 * @param color                 color code used by the native runtime
 * @return                      number of characters written
 */
@native("colored_sprintln")
native inline fun coloredSprintln(dest: pointer<char>, value: pointer<char>, color: int) -> int;
