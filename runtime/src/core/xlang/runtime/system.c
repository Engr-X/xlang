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

#include "xlang/runtime/system.h"

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

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
    #include <windows.h>
#else
    #include <sys/time.h>
#endif

#define XLANG_MS_PER_S 1000LL
#define XLANG_NS_PER_MS 1000000LL
#define XLANG_NS_PER_S 1000000000LL


/**
 * Retrieves the current realtime wall-clock timestamp.
 *
 * This function fills `ts` with the current system realtime clock value. The
 * returned timestamp represents wall-clock time, normally relative to the Unix
 * epoch: 1970-01-01 00:00:00 UTC.
 *
 * On Windows, the implementation reads the system time as a `FILETIME` value
 * and converts it from the Windows epoch to the Unix epoch. If
 * `GetSystemTimePreciseAsFileTime` is available, it is used for better
 * precision. Otherwise, the function falls back to `GetSystemTimeAsFileTime`.
 *
 * On non-Windows platforms, the implementation tries the available realtime
 * APIs in order:
 *
 *   1. `clock_gettime(CLOCK_REALTIME, ...)`
 *   2. `timespec_get(..., TIME_UTC)`
 *   3. `gettimeofday(...)`
 *
 * The returned value is wall-clock time, not monotonic time. It may move
 * forwards or backwards if the operating system clock is adjusted.
 *
 * If `ts` is null, or if no timestamp source is available, or if all timestamp
 * calls fail, this function writes no valid timestamp and returns 0. When a
 * non-null `ts` is provided but no timestamp can be retrieved, the function
 * writes zero seconds and zero nanoseconds into `ts`.
 *
 * @param ts output pointer that receives the realtime timestamp
 * @return 1 if a timestamp was retrieved successfully, 0 otherwise
 */
static int xlang_get_realtime_ts(struct timespec* ts)
{
#if defined(_WIN32)
    FILETIME ft;
    ULONGLONG ticks100ns;
    ULONGLONG unix100ns;
    const ULONGLONG unixEpochDiff100ns = 116444736000000000ULL;
    typedef VOID (WINAPI *GetPreciseTimeFn)(LPFILETIME);
    HMODULE k32 = GetModuleHandleA("kernel32.dll");
    GetPreciseTimeFn getPreciseTime = NULL;

    if (k32 != NULL)
    {
        FARPROC rawProc = GetProcAddress(k32, "GetSystemTimePreciseAsFileTime");
        if (rawProc != NULL)
            memcpy(&getPreciseTime, &rawProc, sizeof(getPreciseTime));
    }

    if (getPreciseTime != NULL)
        getPreciseTime(&ft);
    else
        GetSystemTimeAsFileTime(&ft);

    ticks100ns = ((ULONGLONG)(ft.dwHighDateTime) << 32) | (ULONGLONG)ft.dwLowDateTime;
    if (ticks100ns < unixEpochDiff100ns)
    {
        ts -> tv_sec = 0;
        ts -> tv_nsec = 0;
        return 1;
    }

    unix100ns = ticks100ns - unixEpochDiff100ns;
    ts -> tv_sec = (time_t)(unix100ns / 10000000ULL);
    ts -> tv_nsec = (long)((unix100ns % 10000000ULL) * 100ULL);
    return 1;
#else
    #if defined(CLOCK_REALTIME)
    if (clock_gettime(CLOCK_REALTIME, ts) == 0)
        return 1;
    #endif

    #if defined(TIME_UTC)
    if (timespec_get(ts, TIME_UTC) == TIME_UTC)
        return 1;
    #endif

    {
        struct timeval tv;
        if (gettimeofday(&tv, NULL) == 0)
        {
            ts -> tv_sec = tv.tv_sec;
            ts -> tv_nsec = (long)tv.tv_usec * 1000L;
            return 1;
        }
    }

    ts -> tv_sec = 0;
    ts -> tv_nsec = 0;
    return 0;
#endif
}


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
x_i64 xlang_now_s()
{
    struct timespec ts;
    xlang_get_realtime_ts(&ts);
    return (x_i64)(ts.tv_sec);
}


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
x_i64 xlang_now_ms()
{
    struct timespec ts;
    xlang_get_realtime_ts(&ts);

    return (x_i64)(ts.tv_sec) * XLANG_MS_PER_S +
        (x_i64)(ts.tv_nsec) / XLANG_NS_PER_MS;
}


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
x_i64 xlang_now_ns()
{
    struct timespec ts;
    xlang_get_realtime_ts(&ts);

    return (x_i64)(ts.tv_sec) * XLANG_NS_PER_S +
        (x_i64)(ts.tv_nsec);
}
