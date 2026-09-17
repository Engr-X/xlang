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
#include "util.h"


int filesize(const x_char* const path)
{
    if (path == NULL)
        return -1;

    char* narrow_path = (char*)(malloc((xchar_strlen(path) + 1) * sizeof(char)));
    FILE* file;
    long size;

    if (narrow_path == NULL)
        return -1;

    narrow_xchar_string(path, narrow_path);

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

    return (int)(size);
}


int read(x_char* const dest, const x_char* const path)
{
    if (dest == NULL || path == NULL)
        return -1;
    
    char* narrow_path = (char*)(malloc((xchar_strlen(path) + 1) * sizeof(char)));
    char* bytes;
    FILE* file;
    int size;
    size_t read_size;
    size = filesize(path);

    if (size < 0)
    {
        free(narrow_path);
        return size;
    }

    if (narrow_path == NULL)
        return -1;

    narrow_xchar_string(path, narrow_path);
    file = fopen(narrow_path, "rb");
    free(narrow_path);

    if (file == NULL)
        return -2;

    bytes = (char*)(malloc((size_t)size + 1));
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
    if (dest == NULL || path == NULL)
        return 0;

    dest[0] = 0;

    char* narrow_path = (char*)(malloc((xchar_strlen(path) + 1) * sizeof(char)));
    size_t dest_index = 0;
    int first = 1;
    int count = 0;

    if (narrow_path == NULL)
        return 0;

    narrow_xchar_string(path, narrow_path);

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
        narrow_path = NULL;

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
    if (narrow_path != NULL)
    {
        free(narrow_path);
        narrow_path = NULL;
    }

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

    if (narrow_path != NULL)
        free(narrow_path);
    dest[dest_index] = 0;
    return count;
}


bool is_directory(const x_char* const full_path)
{
    if (full_path == NULL)
        return false;

    char* narrow_path = (char*)(malloc((xchar_strlen(full_path) + 1) * sizeof(char)));
    narrow_xchar_string(full_path, narrow_path);

    if (narrow_path == NULL)
    {
        free(narrow_path);
        return false;
    }

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
   

    if (full_path == NULL)
        return false;

    char* narrow_path = (char*)(malloc((xchar_strlen(full_path) + 1) * sizeof(char)));
    narrow_xchar_string(full_path, narrow_path);

    if (narrow_path == NULL)
    {
        free(narrow_path);
        return false;
    }

    
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
