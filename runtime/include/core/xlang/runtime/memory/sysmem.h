/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

#ifndef _XLANG_RUNTIME_SYSMEM_H_
#define _XLANG_RUNTIME_SYSMEM_H_

/**
 * System-level memory allocation declarations for the xlang runtime.
 *
 * This header defines the lowest-level native memory allocation interface used
 * by the xlang runtime. These functions are thin wrappers around the host
 * platform allocator, such as `malloc`, `realloc`, and `free`.
 *
 * Higher-level runtime memory managers should call these functions instead of
 * calling the C standard library allocator directly. This keeps the runtime
 * allocation backend centralized and makes it easier to replace the allocator
 * later for custom platforms, embedded targets, debugging allocators, or
 * garbage-collected runtimes.
 *
 * These functions do not track allocated blocks. Allocation bookkeeping,
 * leak cleanup, ownership management, and runtime shutdown behavior should be
 * implemented by the higher-level memory manager.
 * 
 * @author Di Wang
 * @since alpha-1.1.0
 */

#include "xlang/xtypedef.h"


/**
 * Allocates a raw block of native memory from the host system allocator.
 *
 * This function requests `size` bytes from the underlying platform allocation
 * backend. The returned pointer is not automatically registered in the xlang
 * runtime memory manager. Callers that need tracked allocation should use the
 * higher-level runtime allocation API instead.
 *
 * If the allocation fails, the function returns null.
 *
 * Passing a size less than or equal to zero is implementation-defined unless
 * the source file explicitly handles it. Higher-level callers should normally
 * reject non-positive allocation sizes before calling this function.
 *
 * @param size                  the number of bytes to allocate
 * @return                      a pointer to the allocated memory block, or null if allocation fails
 */
extern void* xlang_sys_alloc(const x_i64 size);


/**
 * Resizes a raw native memory block using the host system allocator.
 *
 * This function changes the size of a memory block previously allocated by
 * `xlang_sys_alloc` or `xlang_sys_realloc`. The returned pointer may be the
 * same as the original pointer or a different pointer if the allocation had to
 * be moved.
 *
 * If reallocation fails, the function returns null and the original memory
 * block remains valid. Callers must not overwrite the original pointer until
 * they have checked that the returned value is not null.
 *
 * This function does not update any higher-level allocation tracking data.
 * Runtime memory managers must update their own bookkeeping after a successful
 * reallocation.
 *
 * @param memory                the memory block to resize, or null to allocate a new block
 * @param size                  the requested new size in bytes
 * @return                      a pointer to the resized memory block, or null if reallocation fails
 */
extern void* xlang_sys_realloc(const void* const memory, const x_i64 size);


/**
 * Releases a raw native memory block back to the host system allocator.
 *
 * This function frees a memory block previously returned by `xlang_sys_alloc`
 * or `xlang_sys_realloc`. It does not remove the pointer from any higher-level
 * runtime tracking structure.
 *
 * Passing null is allowed if the implementation follows the behavior of the C
 * standard library `free`, but callers should not rely on this unless the
 * source file guarantees it.
 *
 * After this function returns, the pointer must not be dereferenced or passed
 * to another free operation.
 *
 * @param ptr                   the memory block to release
 */
extern void xlang_sys_free(const void* const ptr);

#endif
