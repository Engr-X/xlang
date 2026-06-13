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

#include "xlang/runtime/io/file.h"

#include <stdio.h>
#include <limits.h>


int file_size(const char* const path, int* const result)
{
    FILE* file;
    long size;

    if (path == NULL || result == NULL)
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
    
    if (size < 0)
    {
        fclose(file);
        return -4;
    }

    if (size > INT_MAX)
    {
        fclose(file);
        return -5;
    }

    fclose(file);

    *result = (int)(size);
    return 0;
}


/**
 * Reads an entire file into a caller-provided buffer.
 *
 * The file specified by `path` is opened in binary read mode. The function
 * determines the file size, seeks back to the beginning, and reads exactly
 * that many bytes into `dest`. * * After the file contents, the function writes a null terminator to
 * `dest[file_size]`. The destination therefore contains the original file * bytes followed by `'\0'`. The terminator is added for convenience and is
 * not included in the returned byte count. 
 * File contents are copied without character encoding conversion or
 * text-mode newline translation. The file may contain arbitrary binary data,
 * including embedded null bytes; therefore, the resulting buffer is only a
 * conventional C string when the file itself contains no embedded null bytes.
 *
 * The caller is responsible for ensuring that `dest` has space for at least
 * `file_size + 1` bytes. This function cannot verify the capacity of the
 * supplied buffer.
 * The file is closed before the function returns after it has been
 * successfully opened. If an error occurs after reading has started, `dest`
 * may contain partially read data and is not guaranteed to be * null-terminated. 
 * This function relies on `fseek` and `ftell`, so it may not work for
 * non-seekable streams or for files whose sizes cannot be represented by `long`.
 *
 * @param path                  pointer to a valid null-terminated file path
 * @param dest                  pointer to the buffer that receives the file contents and trailing null terminator 
 * 
 * @return                      the number of file bytes written to `dest` on success, excluding
 *                              the trailing null terminator.
 * @return                      `-1` if `path` or `dest` is null.
 * @return                      `-2` if the file cannot be opened.
 * @return                      `-3` if seeking to the end of the file fails.
 * @return                      `-4` if obtaining the file position fails.
 * @return                      `-5` if the file is too large for the return type or for the * trailing null terminator.
 * @return                      `-6` if seeking back to the beginning of the file fails.
 * @return                      `-7` if the complete file contents cannot be read. 
 *
 * @warning                     `path` must point to a valid null-terminated string.
 * @warning                     `dest` must point to writable memory containing at least `file_size + 1` bytes.
 * @warning                     The function does not receive the capacity of `dest`; passing an * undersized buffer causes undefined behavior.
 */
int read_file(const char* const path, char* const dest)
{
    FILE* file;
    long size;
    size_t read_size;

    if (path == NULL || dest == NULL)
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
    if (size < 0)
    {
        fclose(file);
        return -4;
    }

    if (size > INT_MAX - 1)
    {
        fclose(file);
        return -5;
    }

    if (fseek(file, 0, SEEK_SET) != 0)
    {
        fclose(file);
        return -6;
    }

    read_size = fread(dest, 1, (size_t)(size), file);
    if (read_size != (size_t)(size))
    {
        fclose(file);
        return -7;
    }

    dest[read_size] = '\0';

    fclose(file);

    return (int)(read_size);
}
