#ifndef _XLANG_MEMORY_MANAGER_H_
#define _XLANG_MEMORY_MANAGER_H_

#include <stdint.h>


void xlang_minit();

void* xlang_malloc(const int64_t size);

void xlang_mfree(const void* ptr);

void xlang_mexit();


#endif