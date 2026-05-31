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

#ifndef _XTYPE_DEF_H_
#define _XTYPE_DEF_H_

/**
 *  * Fixed-width primitive type definitions for the xlang runtime.
 *
 * This header defines the native C representations of xlang's primitive
 * integer and floating-point types. These aliases provide a stable naming
 * layer between the xlang language/runtime type system and the underlying C
 * implementation.
 *
 * The runtime uses these types whenever a value must have an exact storage
 * width, ABI representation, or compiler-visible layout. Public runtime
 * headers should prefer these aliases over raw C integer types such as
 * `int`, `long`, or `long long`, because the size of those C types can vary
 * between platforms and ABIs.
 *
 * Type naming convention:
 *
 *   x_i8      signed 8-bit integer
 *   x_i16     signed 16-bit integer
 *   x_i32     signed 32-bit integer
 *   x_i64     signed 64-bit integer
 *
 *   x_u8      unsigned 8-bit integer
 *   x_u16     unsigned 16-bit integer
 *   x_u32     unsigned 32-bit integer
 *   x_u64     unsigned 64-bit integer
 *   x_u128    unsigned 128-bit integer, when supported by the compiler
 *
 *   x_f32     32-bit floating-point value
 *   x_f64     64-bit floating-point value
 *
 * The floating-point static assertions make sure that the host C compiler
 * matches the runtime's expected binary representation size for `float` and
 * `double`.
 * 
 * @author Di Wang
 * @since alpha-1.1.0
 */

 
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <float.h>
#include <limits.h>


typedef int8_t      x_i8;
typedef int16_t     x_i16;
typedef int32_t     x_i32;
typedef int64_t     x_i64;

typedef uint8_t     x_u8;
typedef uint16_t    x_u16;
typedef uint32_t    x_u32;
typedef uint64_t    x_u64;
typedef __uint128_t x_u128;

typedef float       x_f32;
typedef double      x_f64;


static_assert(sizeof(x_f32) == 4, "float must be 32-bit");
static_assert(sizeof(x_f64) == 8, "double must be 64-bit");

#endif
