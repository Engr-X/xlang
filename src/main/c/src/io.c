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

#include "io.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <windows.h>
#endif



int enable_ansi_color()
{
#ifdef _WIN32
    HANDLE handle;
    DWORD mode;

    handle = GetStdHandle(STD_OUTPUT_HANDLE);

    if (handle == INVALID_HANDLE_VALUE)
        return -1;

    if (!GetConsoleMode(handle, &mode))
        return -2;

    mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;

    if (!SetConsoleMode(handle, mode))
        return -3;
#endif

    return 0;
}


static int is_valid_color(const int color)
{
    return color == COLOR_RESET ||
           (color >= 31 && color <= 37) ||
           (color >= 90 && color <= 97);
}


static size_t xchar_strlen(const x_char* const value)
{
    size_t length = 0;

    while (value[length] != 0)
        length++;

    return length;
}


static char* narrow_xchar_string(const x_char* const value)
{
    const size_t length = xchar_strlen(value);
    char* const result = malloc(length + 1);

    if (result == NULL)
        return NULL;

    for (size_t i = 0; i < length; i++)
        result[i] = (char)(value[i] & 0xff);

    result[length] = '\0';
    return result;
}


static void widen_c_string(x_char* const dest, const char* const value, const int length)
{
    for (int i = 0; i < length; i++)
        dest[i] = (unsigned char)value[i];

    dest[length] = 0;
}


static int write_colored_string(x_char* const dest, const x_char* const value, const int color, const int newline)
{
    char* narrow_value;
    char* buffer;
    int required;
    int written;

    if (dest == NULL || value == NULL)
        return -1;

    if (!is_valid_color(color))
        return -3;

    narrow_value = narrow_xchar_string(value);
    if (narrow_value == NULL)
        return -4;

    if (color == COLOR_RESET)
        required = snprintf(NULL, 0, newline ? "%s\n" : "%s", narrow_value);
    else
        required = snprintf(NULL, 0, newline ? "\033[%dm%s\033[0m\n" : "\033[%dm%s\033[0m", color, narrow_value);

    if (required < 0)
    {
        free(narrow_value);
        return -4;
    }

    buffer = malloc((size_t)required + 1);
    if (buffer == NULL)
    {
        free(narrow_value);
        return -4;
    }

    if (color == COLOR_RESET)
        written = snprintf(buffer, (size_t)required + 1, newline ? "%s\n" : "%s", narrow_value);
    else
        written = snprintf(buffer, (size_t)required + 1, newline ? "\033[%dm%s\033[0m\n" : "\033[%dm%s\033[0m", color, narrow_value);

    if (written < 0)
    {
        free(buffer);
        free(narrow_value);
        return -4;
    }

    widen_c_string(dest, buffer, written);

    free(buffer);
    free(narrow_value);

    return written;
}


int colored_sprint(x_char* const dest, const x_char* const value, const int color)
{
    return write_colored_string(dest, value, color, 0);
}


int colored_sprintln(x_char* const dest, const x_char* const value, const int color)
{
    return write_colored_string(dest, value, color, 1);
}
