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

#file.outerClass("Regex")
package xlang.util.string


/**
 * Returns the number of bytes required to store one compiled regular
 * expression.
 *
 * @return                  required destination size in bytes
 */
@Native("regex_compile_size")
native inline fun compileSize() -> int


/**
 * Compiles a regular-expression pattern into caller-provided storage.
 *
 * The destination must point to at least compileSize() writable bytes and
 * remains owned by the caller.
 *
 * @param pattern           regular-expression pattern to compile
 * @param dest              destination for the compiled representation
 */
@Native("regex_compile")
native inline fun compile(pattern: pointer<char>, dest: pointer<*>) -> void



@Native("regex_match_compiled")
native inline fun regexMatch(compiled: pointer<*>, str: pointer<char>) -> int
