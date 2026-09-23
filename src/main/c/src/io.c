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
#include "util.h"

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


static void widen_c_string(x_char* const dest, const char* const value, const int length)
{
    for (int i = 0; i < length; i++)
        dest[i] = (unsigned char)value[i];

    dest[length] = 0;
}


static int write_colored_string(x_char* const dest, const x_char* const value, const int color, const int newline)
{
    if (dest == NULL || value == NULL)
        return -1;

    if (!is_valid_color(color))
        return -3;

    char* narrow_value = (char*)(malloc((xchar_strlen(value) + 1) * sizeof(char)));
    char* buffer;
    int required;
    int written;
    
    if (narrow_value == NULL)
        return -4;

    narrow_xchar_string(value, narrow_value);

    if (color == COLOR_RESET)
        required = snprintf(NULL, 0, newline ? "%s\n" : "%s", narrow_value);
    else
        required = snprintf(NULL, 0, newline ? "\033[%dm%s\033[0m\n" : "\033[%dm%s\033[0m", color, narrow_value);

    if (required < 0)
    {
        free(narrow_value);
        return -4;
    }

    buffer = (char*)(malloc((required + 1) * sizeof(char)));
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


int print(const x_char* const str)
{
    if (str == NULL)
        return 0;

    int count = 0;

    for (int i = 0; str[i] != 0; i++)
    {
        uint32_t ch = (uint32_t)str[i];

        if (ch <= 0x7F)
        {
            if (putchar((int)ch) == EOF)
                return -1;

            count++;
        }
        else if (ch <= 0x7FF)
        {
            if (putchar(0xC0 | (ch >> 6)) == EOF)
                return -1;

            if (putchar(0x80 | (ch & 0x3F)) == EOF)
                return -1;

            count += 2;
        }
        else if (ch <= 0xFFFF)
        {
            if (ch >= 0xD800 && ch <= 0xDFFF)
                return -1;

            if (putchar(0xE0 | (ch >> 12)) == EOF)
                return -1;

            if (putchar(0x80 | ((ch >> 6) & 0x3F)) == EOF)
                return -1;

            if (putchar(0x80 | (ch & 0x3F)) == EOF)
                return -1;

            count += 3;
        }
        else if (ch <= 0x10FFFF)
        {
            if (putchar(0xF0 | (ch >> 18)) == EOF)
                return -1;

            if (putchar(0x80 | ((ch >> 12) & 0x3F)) == EOF)
                return -1;

            if (putchar(0x80 | ((ch >> 6) & 0x3F)) == EOF)
                return -1;

            if (putchar(0x80 | (ch & 0x3F)) == EOF)
                return -1;

            count += 4;
        }
        else
        {
            return -1;
        }
    }

    return count;
}


int println(const x_char* const str)
{
    int count = print(str);

    if (count < 0)
        return -1;

    if (putchar('\n') == EOF)
        return -1;

    return count + 1;
}
