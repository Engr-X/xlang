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

#include "util.h"
#include "xlang/xtypedef.h"


int xchar_strlen(const x_char* const value)
{
    int length = 0;

    if (value == NULL)
        return 0;

    while (value[length] != XLANG_NULL_CHAR)
        length++;

    return length;
}


void narrow_xchar_string(const x_char* const value, char* const dest)
{
    const int length = xchar_strlen(value);

    if (dest == NULL)
        return;

    for (int i = 0; i < length; i++)
        dest[i] = (char)(value[i]);

    dest[length] = XLANG_NULL_CHAR;
}
