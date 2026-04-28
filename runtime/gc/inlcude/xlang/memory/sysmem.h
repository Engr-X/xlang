#ifndef _XLANG_SYSMEM_H_
#define _XLANG_SYSMEM_H_

#include <stdint.h>


// implement these method, then use memory manager
void* xlang_sys_alloc(const int64_t size);

void* xlang_sys_realloc(const void* memory, const int64_t size);

void xlang_sys_free(const void* ptr);


#endif
