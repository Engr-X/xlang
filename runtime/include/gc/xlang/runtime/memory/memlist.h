#ifndef _XLANG_RUNTIME_MEMORY_MEMLIST_H_
#define _XLANG_RUNTIME_MEMORY_MEMLIST_H_

#define LIST_INIT_CAPCITY 16
#define LOAD_FACTOR 0.75


#include <stdlib.h>
#include <stdbool.h>

struct memlist
{
    void** list;
    size_t length;
    size_t __capacity;
};


bool memlist_init(struct memlist* const memlist);


size_t memlist_find(struct memlist* const memlist, const void* const memory);


bool memlist_add(struct memlist* const memlist, const void* const memory);


void memlist_remove_at(struct memlist* const memlist, const size_t index);


void memlist_remove(struct memlist* const memlist, const void* const memory);


void memlist_delete(struct memlist* const memlist);

#endif
