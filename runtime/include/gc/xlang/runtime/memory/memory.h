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

#ifndef _XLANG_RUNTIME_MEMORY_MEMORY_H_
#define _XLANG_RUNTIME_MEMORY_MEMORY_H_

/**
 * Public memory management API for the xlang native runtime.
 *
 * This header declares the external memory management functions exposed by the
 * xlang runtime. These functions are intended to be used by runtime modules,
 * standard library native bindings, and compiler-generated native code that
 * needs to allocate memory through the xlang runtime layer.
 *
 * The functions in this file are higher-level than the system allocator
 * wrappers declared in `sysmem.h`. Unlike raw system allocation functions,
 * this API may track allocated memory blocks so that the runtime can release
 * remaining allocations during shutdown.
 *
 * Typical lifecycle:
 *
 *   1. Call `xlang_minit()` before using the runtime memory manager.
 *   2. Allocate memory through `xlang_malloc()`.
 *   3. Optionally release individual blocks through `xlang_mfree()`.
 *   4. Call `xlang_mexit()` when the runtime is no longer needed.
 *
 * This API is currently designed for bootstrap-stage native runtime support.
 * It is not a garbage collector and does not provide object tracing, automatic
 * finalization, compaction, or ownership analysis.
 * 
 * @author Di Wang
 * @since alpha-1.1.0
 */

#include <stdint.h>
#include "xlang/xtypedef.h"


/**
 * Initializes the xlang runtime memory manager.
 *
 * This function prepares the runtime memory tracking state used by
 * `xlang_malloc`, `xlang_mfree`, and `xlang_mexit`. It should be called once
 * before any tracked runtime allocation is performed.
 *
 * If initialization fails, the memory manager must not be used until a
 * successful initialization has occurred.
 *
 * Calling this function more than once without calling `xlang_mexit` first is
 * not guaranteed to be valid unless the implementation explicitly supports
 * reinitialization.
 *
 * @return                      true if the memory manager was initialized successfully, false otherwise
 */
extern bool xlang_minit();


/**
 * Allocates a tracked block of runtime memory.
 *
 * This function allocates `size` bytes through the xlang runtime memory layer.
 * If the allocation succeeds, the returned pointer is registered in the
 * runtime memory manager so that it can be released manually through
 * `xlang_mfree` or automatically during `xlang_mexit`.
 *
 * If allocation fails, or if the allocated block cannot be registered in the
 * runtime tracking list, the function returns null.
 *
 * The returned memory is uninitialized unless the implementation explicitly
 * documents otherwise.
 *
 * @param size                  the number of bytes to allocate
 * @return                      a pointer to the allocated memory block, or null if allocation fails
 */
extern void* xlang_malloc(const x_i32 size);


/**
 * Releases a tracked runtime memory block.
 *
 * This function releases a memory block previously returned by `xlang_malloc`
 * or another tracked runtime allocation function. If the pointer is known to
 * the runtime memory manager, it is removed from the tracking list and then
 * released to the underlying allocator.
 *
 * If the pointer is null, or if the pointer is not managed by the xlang memory
 * manager, the function returns false and does not release it.
 *
 * After this function returns successfully, the pointer must not be
 * dereferenced, reallocated, or freed again.
 *
 * @param ptr                   the tracked memory block to release
 * @return                      true if the memory block was released successfully, false otherwise
 */
void* xlang_realloc(const void* const ptr, const x_i32 size);


/**
 * Releases a tracked runtime memory block.
 *
 * This function releases a memory block previously returned by `xlang_malloc`
 * or another tracked runtime allocation function. If the pointer is known to
 * the runtime memory manager, it is removed from the tracking list and then
 * released to the underlying allocator.
 *
 * If the pointer is null, or if the pointer is not managed by the xlang memory
 * manager, the function returns false and does not release it.
 *
 * After this function returns successfully, the pointer must not be
 * dereferenced, reallocated, or freed again.
 *
 * @param ptr                   the tracked memory block to release
 * @return                      true if the memory block was released successfully, false otherwise
 */
extern bool xlang_mfree(const void* const ptr);


/**
 * Shuts down the xlang runtime memory manager.
 *
 * This function releases all memory blocks that are still tracked by the
 * runtime memory manager. It is intended to be called during runtime shutdown
 * to clean up allocations that were not individually released through
 * `xlang_mfree`.
 *
 * After this function returns, previously allocated tracked pointers must be
 * treated as invalid. The memory manager should not be used again unless it is
 * successfully reinitialized with `xlang_minit`.
 */
extern void xlang_mexit();


#endif