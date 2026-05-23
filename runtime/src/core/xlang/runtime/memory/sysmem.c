#include "xlang/runtime/memory/sysmem.h"

#include <stdlib.h>


void* xlang_sys_alloc(const x_i64 size) 
{
    if (size <= 0)
        return NULL;

    return malloc(size);
}


void* xlang_sys_realloc(const void* const memory, const x_i64 size)
{
    if (size <= 0)
        return NULL;

    const void* new_memory = realloc(memory, size);

    if (new_memory == NULL)
        return NULL;

    return new_memory;
}


void xlang_sys_free(const void* const ptr) 
{
    free((void*)(ptr));
}

