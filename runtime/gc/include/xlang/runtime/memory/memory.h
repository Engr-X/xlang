#ifndef _XLANG_RUNTIME_MEMORY_MEMORY_H_
#define _XLANG_RUNTIME_MEMORY_MEMORY_H_

#include <stdint.h>


void xlang_minit();

void* xlang_malloc(const x_i64 size);

void xlang_mfree(const void* ptr);

void xlang_mexit();


#endif