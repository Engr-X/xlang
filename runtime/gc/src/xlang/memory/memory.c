#include "xlang/memory/memory.h"

#include <stdlib.h>


void* xlang_sys_alloc(const int64_t size) 
{
    if (size <= 0)
        return NULL;

    return malloc(size);
}

void xlang_sys_free(const void* ptr) 
{
    free((void*)(ptr));
}



void xlang_minit() 
{
    
}


void* xlang_malloc(const int64_t size) 
{

}

void* xlang_realloc(const void* ptr, const int64_t size) 
{
    
}


void xlang_mfree(const void* ptr) 
{
   
}


void xlang_mexit()
{
    
}