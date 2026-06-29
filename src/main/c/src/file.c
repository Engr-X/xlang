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


int filesize(const char* const path)
{
    FILE* file;
    long size;

    if (path == NULL)
        return -1;

    file = fopen(path, "rb");

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


int read(char* const dest, const char* const path)
{
    FILE* file;
    int size;
    size_t read_size;

    if (dest == NULL || path == NULL)
        return -1;

    size = filesize(path);

    if (size < 0)
        return size;

    file = fopen(path, "rb");

    if (file == NULL)
        return -2;

    read_size = fread(dest, 1, (size_t)size, file);
    fclose(file);

    if (read_size != (size_t)size)
    {
        dest[read_size] = '\0';
        return -3;
    }

    dest[size] = '\0';

    return size;
}
