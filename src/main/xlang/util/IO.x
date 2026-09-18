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

#file.outerClass("IO")
package xlang.util

import xlang.System
import xlang.util.string.String
import xlang.util.string.StringBuilder


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
 * bytes that readFileToBuffer expects to clone.
 *
 * @param path              pointer to the null-terminated file path
 *
 * @return                  file size in bytes, or a negative native error code
 */
@Native("filesize")
private native inline fun filesize(path: pointer<char>) -> int;


/**
 * Reads a whole file into a caller-provided buffer.
 *
 * This is a low-level native binding used by higher-level file helpers.
 * The destination buffer must have at least filesize(path) + 1 character
 * slots because the native implementation widens each file byte into one
 * char element and appends a final null terminator.
 *
 * @param dest              pointer to the destination character buffer
 * @param path              pointer to the null-terminated file path
 *
 * @return                  number of bytes read, or a negative native error code
 */
@Native("read")
private native inline fun readFileToBuffer(dest: pointer<char>, path: pointer<char>) -> int;


/**
 * Lists the direct children of a directory into a caller-provided buffer.
 *
 * The native implementation writes both files and sub-directories. It only
 * lists entries directly under path and does not recursively walk nested
 * directories. The entry names are written as a single null-terminated string
 * separated by the pipe character {@code |}; there is no trailing separator.
 *
 * The order is the order returned by the operating system and should not be
 * treated as stable.
 *
 * The caller must ensure that dest has enough space for every returned entry
 * name, every separator, and the final null terminator. This function does not
 * receive a capacity argument, so an undersized buffer is undefined behavior.
 *
 * @param path              pointer to the null-terminated directory path
 * @param dest              pointer to the destination character buffer
 *
 * @return                  number of directory entries written to dest
 * @return                  0 if path is null, dest is null, the directory cannot be opened,
 *                              or the directory contains no visible entries
 */
@Native("sub_files")
native inline fun subFiles(path: pointer<char>, dest: pointer<char>) -> int;


/**
 * Checks whether a path exists and refers to a directory.
 *
 * This is a thin native helper. It performs the platform-specific filesystem
 * query and returns false for null input, conversion failure, missing paths,
 * regular files, and other non-directory paths.
 *
 * @param fullPath          pointer to the null-terminated path to inspect
 *
 * @return                  true only when fullPath exists and is a directory
 */
@Native("is_directory")
native inline fun isDirectory(fullPath: pointer<char>) -> bool


/**
 * Checks whether a path exists and refers to a regular file-like entry.
 *
 * This is a thin native helper. It returns false for null input, conversion
 * failure, missing paths, directories, and other paths that should not be
 * opened as ordinary files.
 *
 * @param fullPath          pointer to the null-terminated path to inspect
 *
 * @return                  true only when fullPath exists and is a file
 */
@Native("is_file")
native inline fun isFile(fullPath: pointer<char>) -> bool


/**
 * Reads a whole file into a newly allocated null-terminated buffer.
 *
 * The file is read as raw bytes by the native implementation, so line endings
 * are preserved exactly as they exist on disk. The returned buffer is allocated
 * with one extra character slot for the final null terminator, which makes it
 * usable as a pointer<char> source buffer for tokenizer-style code.
 *
 * Empty files, invalid paths, and allocation failures return null.
 *
 * @param path              pointer to the null-terminated file path
 *
 * @return                  newly allocated file content buffer, or null on failure
 */
fun readFile(path: pointer<char>) -> pointer<char>
{
    val size: int = filesize(path)

    if size <= 0:
        return null

    val buffer: pointer<char> = System.allocMemory((size + 1) * sizeof(char)) as pointer<char>

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
 * @return                  native status code, usually 0 on success
 */
@Native("enable_ansi_color")
native inline fun enableANSIColor() -> int


/**
 * Writes a colored string into a destination buffer.
 *
 * The generated string does not automatically add a line feed.
 * The destination buffer must have enough capacity to store the colored output,
 * including any ANSI escape sequences inserted by the native runtime and the
 * final null terminator.
 *
 * @param dest              pointer to the destination character buffer
 * @param value             pointer to the null-terminated string to format
 * @param color             color code used by the native runtime
 *
 * @return                  number of characters written
 */
@Native("colored_sprint")
native inline fun coloredSprint(dest: pointer<char>, value: pointer<char>, color: int) -> int


/**
 * Writes a colored string followed by a line feed into a destination buffer.
 *
 * The destination buffer must have enough capacity to store the colored output,
 * the line feed, any ANSI escape sequences inserted by the native runtime, and
 * the final null terminator.
 *
 * @param dest              pointer to the destination character buffer
 * @param value             pointer to the null-terminated string to format
 * @param color             color code used by the native runtime
 *
 * @return                  number of characters written
 */
@Native("colored_sprintln")
native inline fun coloredSprintln(dest: pointer<char>, value: pointer<char>, color: int) -> int


/**
 * Reads one line from standard input into dest.
 *
 * @param dest              destination buffer
 * @param capacity          destination capacity in char slots
 *
 * @return                  number of characters read, or -1 on EOF or error
 */
@Native("read_line")
native inline fun readLine(dest: pointer<char>, capacity: int) -> int


/**
 * Splits a filesystem path into non-empty path parts.
 *
 * Both Unix and Windows separators are accepted. Consecutive separators are
 * treated as one boundary, so they do not create empty parts. Root markers are
 * not returned as standalone parts; for example, `/usr/bin` becomes `usr`,
 * `bin`, and `C:\dev\xlang` becomes `C:`, `dev`, `xlang`.
 *
 * Each returned element is a StringBuilder containing one path part.
 *
 * @param fullPath          null-terminated path string
 *
 * @return                  ArrayList of StringBuilder
 */
fun splitPath(fullPath: pointer<char>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(StringBuilder))

    if fullPath == null:
        return result

    var part: pointer<StringBuilder> = new StringBuilder()
    var index: int = 0

    while fullPath[index] != '\0':
    {
        val current: char = fullPath[index]

        if current == '/' || current == '\\':
        {
            if part.length > 0:
            {
                result.push(part)
                part = new StringBuilder()
            }
        }
        else:
            part.append(current)

        index++
    }

    if part.length > 0:
        result.push(part)

    return result
}


/**
 * Extracts the full file name from the specified file path.
 *
 * The returned file name includes the file extension, if one exists.
 * Both Unix-style ('/') and Windows-style ('\') path separators are
 * supported.
 *
 * The function searches backward from the end of the path until the
 * last path separator is found, then copies every character after that
 * separator into a newly created StringBuilder.
 *
 * Examples:
 *
 *     "C:\project\src\main.cpp" -> "main.cpp"
 *     "/home/user/test.txt"     -> "test.txt"
 *     "example.data.bin"        -> "example.data.bin"
 *     "main.cpp"                -> "main.cpp"
 *
 * If fullPath is null, an empty StringBuilder is returned.
 *
 * If the path ends with a path separator, such as:
 *
 *     "C:\project\src\"
 *
 * the returned StringBuilder will be empty because the path does not
 * contain a file name after the final separator.
 *
 * The returned StringBuilder is newly allocated and does not reference
 * or modify the original fullPath buffer.
 *
 * Time complexity: O(n), where n is the length of the path.
 * Space complexity: O(m), where m is the length of the file name.
 *
 * @param fullPath
 *     A null-terminated character pointer containing the full file path.
 *
 * @return
 *     A newly allocated StringBuilder containing the complete file name,
 *     including its extension. Returns an empty StringBuilder if fullPath
 *     is null or the path ends with a separator.
 */
fun getFullFileName(fullPath: pointer<char>) -> pointer<StringBuilder>
{
    val result: pointer<StringBuilder> = new StringBuilder()

    if fullPath == null:
        return result

    val end: int = String.strlen(fullPath)
    var start: int = end

    while start > 0:
    {
        if fullPath[start - 1] == '/' || fullPath[start - 1] == '\\':
            break

        start--
    }

    while start < end:
    {
        result.append(fullPath[start])
        start++
    }

    return result
}


/**
 * Extracts the file name from the specified path without its extension.
 *
 * This function first obtains the complete file name by calling
 * getFullFileName(), then searches backward for the last '.' character.
 * If an extension is found, only the portion before the extension is
 * copied into the returned StringBuilder.
 *
 * Examples:
 *
 *     "C:\project\src\main.cpp" -> "main"
 *     "/home/user/test.txt"     -> "test"
 *     "archive.tar.gz"          -> "archive.tar"
 *     "README"                  -> "README"
 *     ".gitignore"              -> ".gitignore"
 *
 * A leading '.' is treated as part of the file name rather than as an
 * extension separator.
 *
 * @param fullPath
 *     A null-terminated character pointer containing the file path.
 *
 * @return
 *     A newly allocated StringBuilder containing the file name without
 *     its extension.
 */
fun getFileName(fullPath: pointer<char>) -> pointer<StringBuilder>
{
    val fullFileName: pointer<StringBuilder> = getFullFileName(fullPath)
    val result: pointer<StringBuilder> = new StringBuilder()

    var end: int = fullFileName.length
    var extension: int = end

    // Search backward for the last extension separator.
    while extension > 0:
    {
        if fullFileName.get(extension - 1) == '.':
        {
            // A leading '.' belongs to the file name itself.
            if extension - 1 > 0:
                end = extension - 1

            break
        }

        extension--
    }

    // Copy the file name without the extension.
    var index: int = 0

    while index < end:
    {
        result.append(fullFileName.get(index))
        index++
    }

    return result
}
