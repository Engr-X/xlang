#include "xlang/runtime/memory/sysmem.h"
#include "xlang/runtime/memory/memlist.h"

#include <string.h>


bool memlist_init(struct memlist* const memlist)
{
    if (memlist == NULL)
        return false;

    memlist -> length = 0;
    memlist -> __capacity = LIST_INIT_CAPCITY;
    memlist -> list = (void**)(xlang_sys_alloc(sizeof(void*) * memlist -> __capacity));

    return true;
}


static bool memlist_resize(struct memlist* const memlist)
{
    if (memlist == NULL)
        return false;

    size_t new_capacity = memlist -> __capacity * 2;

    void** new_list = (void**)(xlang_sys_realloc(
        memlist -> list,
        sizeof(void*) * new_capacity
    ));

    if (new_list == NULL)
        return false;

    memlist -> list = new_list;
    memlist -> __capacity = new_capacity;

    return true;
}


size_t memlist_find(struct memlist* const memlist, const void* const memory)
{
    if (memlist == NULL || memory == NULL)
        return (size_t)(-1);

    for (size_t i = 0; i < memlist -> length; i++)
    {
        if (memlist -> list[i] == memory)
            return i;
    }

    return (size_t)(-1);
}


bool memlist_add(struct memlist* const memlist, const void* const memory)
{
    if (memlist == NULL || memory == NULL)
        return false;

    if (memlist -> length + 1 >= memlist -> __capacity * LOAD_FACTOR)
    {
        if (!memlist_resize(memlist))
            return false;
    }

    memlist -> list[memlist -> length++] = (void*)(memory);
    return true;
}


void memlist_remove_at(struct memlist* const memlist, const size_t index)
{
    if (memlist == NULL || index >= memlist->length)
        return;

    if (index + 1 < memlist->length)
    {
        memmove(
            &memlist -> list[index],
            &memlist -> list[index + 1],
            sizeof(void*) * (memlist -> length - index - 1)
        );
    }

    memlist -> length--;
    memlist -> list[memlist -> length] = NULL;
}


void memlist_remove(struct memlist* const memlist, const void* const memory)
{
    if (memlist == NULL || memory == NULL)
        return;

    const size_t index = memlist_find(memlist, memory);

    if (index == (size_t)-1)
        return;

    memlist_remove_at(memlist, index);
}


void memlist_delete(struct memlist* const memlist)
{
    xlang_sys_free(memlist -> list);
}
