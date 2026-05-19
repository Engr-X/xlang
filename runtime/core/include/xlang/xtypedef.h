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
