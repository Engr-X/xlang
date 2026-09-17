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

#ifndef _XLANG_UTIL_STRING_REGEX_
#define _XLANG_UTIL_STRING_REGEX_


#include <stddef.h>

#include "regex.h"
#include "xlang/xtypedef.h"


x_i32 regex_match(const x_char* const pattern, const x_char* const str);


x_i32 regex_match_compiled(struct regex_t* pattern, const x_char* const str);


x_i32 regex_compile_size(void);


void regex_compile(const x_char* const pattern, struct regex_t* dest);


#endif
