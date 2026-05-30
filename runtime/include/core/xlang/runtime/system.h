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

#ifndef _XLANG_RUNTIME_SYSTEM_H_
#define _XLANG_RUNTIME_SYSTEM_H_

/**
 * Native system API declarations for the xlang runtime.
 *
 * This header defines low-level system functions exposed by the native
 * runtime. These functions are intended to be used by the xlang standard
 * library or by compiler-generated runtime calls, rather than by ordinary
 * user code directly.
 *
 * The current API provides access to wall-clock time in seconds,
 * milliseconds, and nanoseconds. Higher-level time abstractions, date/time
 * formatting, monotonic timers, sleeping, process control, environment
 * variables, and platform-specific system services should be implemented
 * above this layer or added as separate runtime modules.
 *
 * Unless explicitly documented by the implementation, the returned timestamps
 * should be treated as system wall-clock time. Wall-clock time may move
 * forwards or backwards if the operating system clock is adjusted.
 * 
 * @author Di Wang
 * @since alpha-1.1.0
 */

#include "xlang/xtypedef.h"


/**
 * Returns the current system time in seconds.
 *
 * The returned value represents the current wall-clock time measured in
 * seconds. The exact epoch is defined by the runtime implementation, but it is
 * normally expected to be the Unix epoch: 1970-01-01 00:00:00 UTC.
 *
 * This function is suitable for coarse timestamp generation, logging, and
 * simple time comparisons where second-level precision is enough.
 *
 * The value should not be used as a monotonic timer. System clock changes may
 * affect the returned value.
 *
 * @return the current system time in seconds
 */
extern x_i64 xlang_now_s();


/**
 * Returns the current system time in milliseconds.
 *
 * The returned value represents the current wall-clock time measured in
 * milliseconds. The exact epoch is defined by the runtime implementation, but
 * it is normally expected to be the Unix epoch: 1970-01-01 00:00:00 UTC.
 *
 * This function is suitable for higher-resolution timestamps, profiling output,
 * simple timeout calculations, and runtime diagnostics.
 *
 * The value should not be treated as strictly monotonic. If the operating
 * system clock is adjusted, the returned value may move forwards or backwards.
 *
 * @return the current system time in milliseconds
 */
extern x_i64 xlang_now_ms();


/**
 * Returns the current system time in nanoseconds.
 *
 * The returned value represents the current wall-clock time measured in
 * nanoseconds. The exact epoch is defined by the runtime implementation, but it
 * is normally expected to be the Unix epoch: 1970-01-01 00:00:00 UTC.
 *
 * The actual precision depends on the host operating system and the underlying
 * clock source. Even though the unit is nanoseconds, the runtime may not be
 * able to provide true nanosecond precision on every platform.
 *
 * This function should not be used as a monotonic timer unless the
 * implementation explicitly documents monotonic behavior.
 *
 * @return the current system time in nanoseconds
 */
extern x_i64 xlang_now_ns();

#endif
