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

@class("Console")
package xlang.io

/**
 * Console is the standard output facade in the xlang standard library.
 * It exposes native printing functions that write to process stdout.
 *
 * Behavior summary:
 * - put(...): writes text without appending a newline
 * - putln(...): writes text and appends a newline
 * - numeric/boolean/pointer values are rendered using xlang.String.valueOf(...)
 *
 * This type only provides output APIs and does not manage input streams,
 * formatting templates, or buffering policies.
 *
 * @author  Di Wang
 * @since   alpha-1.1.0
 */


/**
 * Prints a boolean value without a trailing newline.
 * The text form follows xlang.String.valueOf(value) and is written to stdout.
 *
 * @param value     boolean value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: bool) -> void


/**
 * Prints one character without a trailing newline.
 *
 * @param value     character value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: char) -> void


/**
 * Prints an int8 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int8 value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: byte) -> void


/**
 * Prints an int16 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int16 value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: short) -> void


/**
 * Prints an int32 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int32 value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: int) -> void

/**
 * Prints an int64 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int64 value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: long) -> void


/**
 * Prints a float32 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     float32 value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: float) -> void


/**
 * Prints a float64 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     float64 value to print
 */
@native("xlang.io.ConsoleRt.print")
native inline fun put(value: double) -> void


/**
 * Prints a boolean value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     boolean value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: bool) -> void


/**
 * Prints one character and then prints a newline.
 *
 * @param value     character value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: char) -> void


/**
 * Prints an int8 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int8 value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: byte) -> void


/**
 * Prints an int16 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int16 value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: short) -> void


/**
 * Prints an int32 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int32 value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: int) -> void


/**
 * Prints an int64 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     int64 value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: long) -> void


/**
 * Prints a float32 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     float32 value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: float) -> void


/**
 * Prints a float64 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value     float64 value to print
 */
@native("xlang.io.ConsoleRt.println")
native inline fun putln(value: double) -> void
