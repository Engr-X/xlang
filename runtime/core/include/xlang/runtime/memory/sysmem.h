#ifndef _XLANG_RUNTIME_SYSMEM_H_
#define _XLANG_RUNTIME_SYSMEM_H_

#include "xlang/xtypedef.h"


// implement these method, then use memory manager
extern void* xlang_sys_alloc(const x_i64 size);

extern void* xlang_sys_realloc(const void* const memory, const x_i64 size);

extern void xlang_sys_free(const void* const ptr);


#endif
