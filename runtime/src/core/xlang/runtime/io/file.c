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
