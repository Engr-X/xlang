#include "xlang/memory/mchunk.h"
#include "xlang/memory/sysmem.h"

#include <stdlib.h>
#include <string.h>

#define LIST_THRESHOLD 0.75
#define LIST_INIT_CAPACITY 128


struct chunk_list chunk_list_create()
{
    struct chunk_list list = {
        .capacity = LIST_INIT_CAPACITY,
        .size = 0,
        .chunks = xlang_sys_alloc(LIST_INIT_CAPACITY * sizeof(struct chunk))
    };

    return list;
}

void chunk_list_resize(struct chunk_list* list)
{
    list -> capacity *= 2;
    list -> chunks = xlang_sys_realloc(list -> chunks, list -> capacity * sizeof(struct chunk));
}


void chunk_list_push(struct chunk_list* list, const struct chunk chunk)
{
    if (list -> size * LIST_THRESHOLD >= list -> capacity)
        chunk_list_resize(list);

    list -> chunks[list -> size++] = chunk;
}

struct chunk chunk_alloc(const struct chunk_info info, const struct chunk_list* list)
{
    
    const struct chunk chunk = {
        .info = info,
        .memory = xlang_sys_alloc(info.class_size * info.slot)
    };

    chunk_list_push(list, chunk);

    return chunk;
}


void chunk_delete(const struct chunk chunk)
{
    xlang_sys_free(chunk.memory);
}
