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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dirent.h>
#include <sys/stat.h>
#endif

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


int sub_files(const x_char* const path, x_char* const dest)
{
    char* narrow_path;
    size_t dest_index = 0;
    int first = 1;
    int count = 0;

    if (dest == NULL)
        return 0;

    dest[0] = 0;

    if (path == NULL)
        return 0;

    narrow_path = narrow_xchar_string(path);
    if (narrow_path == NULL)
        return 0;

#ifdef _WIN32
    {
        const size_t path_length = strlen(narrow_path);
        const int need_separator =
            path_length > 0 && narrow_path[path_length - 1] != '/' && narrow_path[path_length - 1] != '\\';
        const size_t pattern_length = path_length + (need_separator ? 2 : 1);
        char* const pattern = malloc(pattern_length + 1);
        WIN32_FIND_DATAA find_data;
        HANDLE handle;

        if (pattern == NULL)
        {
            free(narrow_path);
            return 0;
        }

        memcpy(pattern, narrow_path, path_length);

        if (need_separator)
        {
            pattern[path_length] = '\\';
            pattern[path_length + 1] = '*';
        }
        else
        {
            pattern[path_length] = '*';
        }

        pattern[pattern_length] = 0;
        free(narrow_path);

        handle = FindFirstFileA(pattern, &find_data);
        free(pattern);

        if (handle == INVALID_HANDLE_VALUE)
            return 0;

        do
        {
            const char* const name = find_data.cFileName;

            if (name[0] == '.' && name[1] == 0)
                continue;

            if (name[0] == '.' && name[1] == '.' && name[2] == 0)
                continue;

            if (first)
                first = 0;
            else
                dest[dest_index++] = '|';

            for (size_t i = 0; name[i] != 0; i++)
                dest[dest_index++] = (unsigned char)name[i];

            count++;
        }
        while (FindNextFileA(handle, &find_data) != 0);

        FindClose(handle);
    }
#else
    {
    DIR* dir;
    struct dirent* entry;

    dir = opendir(narrow_path);
    free(narrow_path);

    if (dir == NULL)
        return 0;

    while ((entry = readdir(dir)) != NULL)
    {
        const char* const name = entry->d_name;

        if (name[0] == '.' && name[1] == 0)
            continue;

        if (name[0] == '.' && name[1] == '.' && name[2] == 0)
            continue;

        if (first)
            first = 0;
        else
            dest[dest_index++] = '|';

        for (size_t i = 0; name[i] != 0; i++)
            dest[dest_index++] = (unsigned char)name[i];

        count++;
    }

    closedir(dir);
    }
#endif

    dest[dest_index] = 0;
    return count;
}


bool is_directory(const x_char* const full_path)
{
    char* narrow_path;

    if (full_path == NULL)
        return false;

    narrow_path = narrow_xchar_string(full_path);
    if (narrow_path == NULL)
        return false;

#ifdef _WIN32
    {
        const DWORD attributes = GetFileAttributesA(narrow_path);
        free(narrow_path);

        return attributes != INVALID_FILE_ATTRIBUTES && (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
    }
#else
    {
        struct stat info;
        const int status = stat(narrow_path, &info);
        free(narrow_path);

        return status == 0 && S_ISDIR(info.st_mode);
    }
#endif
}


bool is_file(const x_char* const full_path)
{
    char* narrow_path;

    if (full_path == NULL)
        return false;

    narrow_path = narrow_xchar_string(full_path);
    if (narrow_path == NULL)
        return false;

#ifdef _WIN32
    {
        const DWORD attributes = GetFileAttributesA(narrow_path);
        free(narrow_path);

        return attributes != INVALID_FILE_ATTRIBUTES && (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0;
    }
#else
    {
        struct stat info;
        const int status = stat(narrow_path, &info);
        free(narrow_path);

        return status == 0 && S_ISREG(info.st_mode);
    }
#endif
}
