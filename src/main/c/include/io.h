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

#ifndef _XLANG_UTIL_IO_
#define _XLANG_UTIL_IO_

#define COLOR_RESET   0
#define COLOR_RED     31
#define COLOR_GREEN   32
#define COLOR_YELLOW  33
#define COLOR_BLUE    34
#define COLOR_MAGENTA 35
#define COLOR_CYAN    36
#define COLOR_WHITE   37

#define COLOR_BRIGHT_BLACK   90
#define COLOR_BRIGHT_RED     91
#define COLOR_BRIGHT_GREEN   92
#define COLOR_BRIGHT_YELLOW  93
#define COLOR_BRIGHT_BLUE    94
#define COLOR_BRIGHT_MAGENTA 95
#define COLOR_BRIGHT_CYAN    96
#define COLOR_BRIGHT_WHITE   97


int enable_ansi_color();


int colored_sprint(char* const dest, const char* const value, const int color);


int colored_sprintln(char* const dest, const char* const value, const int color);

#endif
