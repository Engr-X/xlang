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

#ifndef _XLANG_IO_CONSOLE_H_
#define _XLANG_IO_CONSOLE_H_

#include <stdbool.h>
#include <stdint.h>

/**
 * Console is the standard output facade in the xlang standard library.
 * It exposes native printing functions that write to process stdout.
 *
 * Behavior summary:
 * - xlang_put(...): writes text without appending a newline
 * - xlang_putln(...): writes text and appends a newline
 * - numeric/boolean/pointer values are rendered as textual forms
 *
 * This API only provides output and does not manage input streams,
 * formatting templates, or buffering policies.
 */


/**
 * Prints a boolean value without a trailing newline.
 *
 * @param value boolean value to print
 */
extern void xlang_put_bool(const bool value);


/**
 * Prints one character without a trailing newline.
 *
 * @param value character value to print
 */
extern void xlang_put_char(const char value);


/**
 * Prints an int8 value without a trailing newline.
 *
 * @param value int8 value to print
 */
extern void xlang_put_i8(const int8_t value);


/**
 * Prints an int16 value without a trailing newline.
 *
 * @param value int16 value to print
 */
extern void xlang_put_i16(const int16_t value);


/**
 * Prints an int32 value without a trailing newline.
 *
 * @param value int32 value to print
 */
extern void xlang_put_i32(const int32_t value);


/**
 * Prints an int64 value without a trailing newline.
 *
 * @param value int64 value to print
 */
extern void xlang_put_i64(const int64_t value);


/**
 * Prints a float32 value without a trailing newline.
 *
 * @param value float32 value to print
 */
extern void xlang_put_f32(const float value);


/**
 * Prints a float64 value without a trailing newline.
 *
 * @param value float64 value to print
 */
extern void xlang_put_f64(const double value);


/**
 * Prints a pointer value without a trailing newline.
 *
 * @param value pointer value to print
 */
extern void xlang_put_p(const void* value);


/**
 * Prints a C string without a trailing newline.
 *
 * @param value UTF-8/ASCII C string pointer
 */
extern void xlang_put_str(const char* value);


/**
 * Prints a boolean value and then prints a newline.
 *
 * @param value boolean value to print
 */
extern void xlang_putln_bool(const bool value);


/**
 * Prints one character and then prints a newline.
 *
 * @param value character value to print
 */
extern void xlang_putln_char(const char value);


/**
 * Prints an int8 value and then prints a newline.
 *
 * @param value int8 value to print
 */
extern void xlang_putln_i8(const int8_t value);


/**
 * Prints an int16 value and then prints a newline.
 *
 * @param value int16 value to print
 */
extern void xlang_putln_i16(const int16_t value);


/**
 * Prints an int32 value and then prints a newline.
 *
 * @param value int32 value to print
 */
extern void xlang_putln_i32(const int32_t value);


/**
 * Prints an int64 value and then prints a newline.
 *
 * @param value int64 value to print
 */
extern void xlang_putln_i64(const int64_t value);


/**
 * Prints a float32 value and then prints a newline.
 *
 * @param value float32 value to print
 */
extern void xlang_putln_f32(const float value);


/**
 * Prints a float64 value and then prints a newline.
 *
 * @param value float64 value to print
 */
extern void xlang_putln_f64(const double value);


/**
 * Prints a pointer value and then prints a newline.
 *
 * @param value pointer value to print
 */
extern void xlang_putln_p(const void* value);


/**
 * Prints a C string and then prints a newline.
 *
 * @param value UTF-8/ASCII C string pointer
 */
extern void xlang_putln_str(const char* value);


#endif
