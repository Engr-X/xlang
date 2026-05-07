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
 * Native implementation unit for xlang System.
 * Keep this file as the anchor for future system-level runtime APIs.
 */

#include <time.h>
#include <stdio.h>
#include <string.h>

#if defined(_WIN32)
    #include <windows.h>
#else
    #include <sys/time.h>
#endif

#define XLANG_MS_PER_S 1000LL
#define XLANG_NS_PER_MS 1000000LL
#define XLANG_NS_PER_S 1000000000LL


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


int64_t xlang_now_s()
{
    struct timespec ts;
    xlang_get_realtime_ts(&ts);
    return (int64_t)(ts.tv_sec);
}


int64_t xlang_now_ms()
{
    struct timespec ts;
    xlang_get_realtime_ts(&ts);

    return (int64_t)(ts.tv_sec) * XLANG_MS_PER_S +
        (int64_t)(ts.tv_nsec) / XLANG_NS_PER_MS;
}


int64_t xlang_now_ns()
{
    struct timespec ts;
    xlang_get_realtime_ts(&ts);

    return (int64_t)(ts.tv_sec) * XLANG_NS_PER_S +
        (int64_t)(ts.tv_nsec);
}
