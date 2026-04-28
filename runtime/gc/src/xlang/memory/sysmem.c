#include "xlang/memory/sysmem.h"


void* xlang_sys_alloc(const int64_t size) 
{
    if (size <= 0)
        return NULL;

    return malloc(size);
}


void* xlang_sys_realloc(const void* memory, const int64_t size)
{
    if (size <= 0)
        return NULL;

    const void* new_memory = realloc(memory, size);

    if (new_memory == NULL)
        return NULL;

    return new_memory;
}


void xlang_sys_free(const void* ptr) 
{
    free((void*)(ptr));
}

