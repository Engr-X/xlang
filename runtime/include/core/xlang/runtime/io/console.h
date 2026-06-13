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

#ifndef _XLANG_RUNTIME_CONSOLE_H_
#define _XLANG_RUNTIME_CONSOLE_H_

/**
 * Provides low-level console output functions for the xlang runtime.
 *
 * The functions in this header are native runtime entry points. They are
 * designed to be called by compiler-generated code or by higher-level
 * standard library wrappers such as `xlang_put(...)` and `xlang_putln(...)`.
 *
 * Each `xlang_put_*` function writes a value to the process standard output
 * stream without appending a trailing newline. Each `xlang_putln_*` function
 * writes the same textual representation and then appends a newline.
 *
 * This API only handles output. It does not provide input operations,
 * formatting templates, locale-sensitive formatting, custom stream routing,
 * or explicit buffering control.
 *
 * Null string handling is implementation-defined unless specified by the
 * corresponding source file. Callers should avoid passing null strings unless
 * the runtime implementation explicitly supports them.
 * 
 * @author Di Wang
 * @since alpha-1.1.0
 */

#include "xlang/xtypedef.h"


/**
 * Prints a boolean value without appending a trailing newline.
 *
 * The value is converted to the runtime's textual boolean representation
 * before being written to standard output. The exact spelling is defined by
 * the runtime implementation, but it is normally expected to be similar to
 * `true` or `false`.
 *
 * @param value                 the boolean value to print
 */
extern void xlang_put_bool(const bool value);


/**
 * Prints a single character without appending a trailing newline.
 *
 * The character is written directly to standard output as one character value.
 * This function does not add quotes, escaping, or formatting around the
 * character.
 *
 * @param value                 the character value to print
 */
extern void xlang_put_char(const char value);


/**
 * Prints a 8-bit signed integer without appending a trailing newline.
 *
 * The integer is converted to its decimal textual representation before being
 * written to standard output.
 *
 * @param value                 the 8-bit signed integer value to print
 */
extern void xlang_put_i8(const x_i8 value);


/**
 * Prints a 16-bit signed integer without appending a trailing newline.
 *
 * The integer is converted to its decimal textual representation before being
 * written to standard output.
 *
 * @param value                 the 16-bit signed integer value to print
 */
extern void xlang_put_i16(const x_i16 value);


/**
 * Prints a 32-bit signed integer without appending a trailing newline.
 *
 * The integer is converted to its decimal textual representation before being
 * written to standard output.
 *
 * @param value                 the 32-bit signed integer value to print
 */
extern void xlang_put_i32(const x_i32 value);


/**
 * Prints a 64-bit signed integer without appending a trailing newline.
 *
 * The integer is converted to its decimal textual representation before being
 * written to standard output.
 *
 * @param value                 the 64-bit signed integer value to print
 */
extern void xlang_put_i64(const x_i64 value);


/**
 * Prints a 32-bit floating-point value without appending a trailing newline.
 *
 * The value is converted to a textual floating-point representation before
 * being written to standard output. The precision and formatting style are
 * defined by the runtime implementation.
 *
 * @param value                 the 32-bit floating-point value to print
 */
extern void xlang_put_f32(const x_f32 value);


/**
 * Prints a 64-bit floating-point value without appending a trailing newline.
 *
 * The value is converted to a textual floating-point representation before
 * being written to standard output. The precision and formatting style are
 * defined by the runtime implementation.
 *
 * @param value                 the 64-bit floating-point value to print
 */
extern void xlang_put_f64(const x_f64 value);


/**
 * Prints a pointer value without appending a trailing newline.
 *
 * The pointer is converted to a implementation-defined address string before
 * being written to standard output. This function is intended for diagnostics,
 * debugging, and low-level runtime output.
 *
 * @param value                 the pointer value to print
 */
extern void xlang_put_p(const void* value);


/**
 * Prints a null-terminated C string without appending a trailing newline.
 *
 * The string is written from the given pointer until the first null terminator.
 * The bytes are treated as a runtime string payload, normally UTF-8 or ASCII.
 *
 * Passing a null pointer is not guaranteed to be valid unless the runtime
 * implementation explicitly documents null string support.
 *
 * @param value                 the null-terminated string to print
 */
extern void xlang_put_str(const char* const value);


/**
 * Prints a boolean value followed by a newline.
 *
 * This function has the same value conversion behavior as `xlang_put_bool`,
 * but appends a newline after the textual boolean representation.
 *
 * @param value                 the boolean value to print
 */
extern void xlang_putln_bool(const bool value);


/**
 * Prints a single character followed by a newline.
 *
 * This function has the same character output behavior as `xlang_put_char`,
 * but appends a newline after the character.
 *
 * @param value                 the character value to print
 */
extern void xlang_putln_char(const char value);


/**
 * Prints a 8-bit signed integer followed by a newline.
 *
 * This function has the same integer conversion behavior as `xlang_put_i8`,
 * but appends a newline after the decimal representation.
 *
 * @param value                 the 8-bit signed integer value to print
 */
extern void xlang_putln_i8(const x_i8 value);


/**
 * Prints a 16-bit signed integer followed by a newline.
 *
 * This function has the same integer conversion behavior as `xlang_put_i16`,
 * but appends a newline after the decimal representation.
 *
 * @param value                 the 16-bit signed integer value to print
 */
extern void xlang_putln_i16(const x_i16 value);


/**
 * Prints a 32-bit signed integer followed by a newline.
 *
 * This function has the same integer conversion behavior as `xlang_put_i32`,
 * but appends a newline after the decimal representation.
 *
 * @param value                 the 32-bit signed integer value to print
 */
extern void xlang_putln_i32(const x_i32 value);


/**
 * Prints a 64-bit signed integer followed by a newline.
 *
 * This function has the same integer conversion behavior as `xlang_put_i64`,
 * but appends a newline after the decimal representation.
 *
 * @param value                 the 64-bit signed integer value to print
 */
extern void xlang_putln_i64(const x_i64 value);


/**
 * Prints a 32-bit floating-point value followed by a newline.
 *
 * This function has the same floating-point conversion behavior as
 * `xlang_put_f32`, but appends a newline after the textual representation.
 *
 * @param valuethe              the 32-bit floating-point value to print
 */
extern void xlang_putln_f32(const x_f32 value);


/**
 * Prints a 64-bit floating-point value followed by a newline.
 *
 * This function has the same floating-point conversion behavior as
 * `xlang_put_f64`, but appends a newline after the textual representation.
 *
 * @param value                 the 64-bit floating-point value to print
 */
extern void xlang_putln_f64(const x_f64 value);


/**
 * Prints a pointer value followed by a newline.
 *
 * This function has the same pointer conversion behavior as `xlang_put_p`,
 * but appends a newline after the address representation.
 *
 * @param value                 the pointer value to print
 */
extern void xlang_putln_p(const void* value);


/**
 * Prints a null-terminated C string followed by a newline.
 *
 * This function has the same string output behavior as `xlang_put_str`,
 * but appends a newline after the string contents.
 *
 * Passing a null pointer is not guaranteed to be valid unless the runtime
 * implementation explicitly documents null string support.
 *
 * @param value                 the null-terminated string to print
 */
extern void xlang_putln_str(const char* const value);

#endif
