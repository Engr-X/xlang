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

#ifndef _XLANG_RUNTIME_STACKTRACE_H_
#define _XLANG_RUNTIME_STACKTRACE_H_

#ifndef XLANG_STACKTRACE_MAX_DEPTH
#define XLANG_STACKTRACE_MAX_DEPTH 256
#define XLANG_STACKTRACE_NAME_MAX 1024
#endif

#include "xlang/xtypedef.h"


extern x_i64 get_stacktrace_size(void);

extern void get_stacktrace(x_i64* dest);

#endif
