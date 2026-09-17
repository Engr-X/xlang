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
 */

#ifndef _XLANG_UTIL_FILE_H_
#define _XLANG_UTIL_FILE_H_

#include "xlang/xtypedef.h"


int filesize(const x_char* const path);


int read(x_char* const dest, const x_char* const path);


int sub_files(const x_char* const path, x_char* const dest);


#endif
