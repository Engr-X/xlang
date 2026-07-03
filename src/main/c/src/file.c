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

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "xlang/xtypedef.h"


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


int filesize(const x_char* const path)
{
    char* narrow_path;
    FILE* file;
    long size;

    if (path == NULL)
        return -1;

    narrow_path = narrow_xchar_string(path);
    if (narrow_path == NULL)
        return -1;

    file = fopen(narrow_path, "rb");
    free(narrow_path);

    if (file == NULL)
        return -2;

    if (fseek(file, 0, SEEK_END) != 0)
    {
        fclose(file);
        return -3;
    }

    size = ftell(file);
    fclose(file);

    if (size < 0 || size > INT_MAX)
        return -4;

    return (int)size;
}


int read(x_char* const dest, const x_char* const path)
{
    char* narrow_path;
    char* bytes;
    FILE* file;
    int size;
    size_t read_size;

    if (dest == NULL || path == NULL)
        return -1;

    size = filesize(path);

    if (size < 0)
        return size;

    narrow_path = narrow_xchar_string(path);
    if (narrow_path == NULL)
        return -1;

    file = fopen(narrow_path, "rb");
    free(narrow_path);

    if (file == NULL)
        return -2;

    bytes = malloc((size_t)size + 1);
    if (bytes == NULL)
    {
        fclose(file);
        return -4;
    }

    read_size = fread(bytes, 1, (size_t)size, file);
    fclose(file);

    if (read_size != (size_t)size)
    {
        for (size_t i = 0; i < read_size; i++)
            dest[i] = (unsigned char)bytes[i];

        dest[read_size] = 0;
        free(bytes);
        return -3;
    }

    for (int i = 0; i < size; i++)
        dest[i] = (unsigned char)bytes[i];

    dest[size] = 0;
    free(bytes);

    return size;
}
