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


int colored_sprint(char* const dest, const char* const value, const int color)
{
    int written;

    if (dest == NULL || value == NULL)
        return -1;

    if (!is_valid_color(color))
        return -3;

    if (color == COLOR_RESET)
        written = sprintf(dest, "%s", value);
    else
        written = sprintf(dest, "\033[%dm%s\033[0m", color, value);

    if (written < 0)
        return -4;

    return written;
}


int colored_sprintln(char* const dest, const char* const value, const int color)
{
    int written;

    if (dest == NULL || value == NULL)
        return -1;

    if (!is_valid_color(color))
        return -3;

    if (color == COLOR_RESET)
        written = sprintf(dest, "%s\n", value);
    else
        written = sprintf(dest, "\033[%dm%s\033[0m\n", color, value);

    if (written < 0)
        return -4;

    return written;
}
