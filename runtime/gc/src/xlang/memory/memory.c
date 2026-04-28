#include "xlang/memory/memory.h"
#include "xlang/memory/mchunk.h"

#include <stdlib.h>



struct memory_manager
{
    struct chunk_list list;
};


extern struct memory_manager manager;

void xlang_minit() 
{
    manager.list = chunk_list_create();
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
    struct chunk* ptr = manager.list.chunks;

    for (int i = 0; i < manager.list.size; i++)
        
}