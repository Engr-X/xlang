#include "xlang/runtime/stacktrace.h"

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>


#if defined(_WIN32)
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
#elif defined(__unix__) || defined(__APPLE__)
    #if defined(__has_include)
        #if __has_include(<execinfo.h>)
            #include <execinfo.h>
            #define XLANG_HAS_EXECINFO 1
        #endif
    #else
        #include <execinfo.h>
        #define XLANG_HAS_EXECINFO 1
    #endif
#endif

#if defined(_MSC_VER)
    #define XLANG_THREAD_LOCAL __declspec(thread)
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    #define XLANG_THREAD_LOCAL _Thread_local
#elif defined(__GNUC__) || defined(__clang__)
    #define XLANG_THREAD_LOCAL __thread
#else
    #define XLANG_THREAD_LOCAL
#endif


static XLANG_THREAD_LOCAL x_i64 cached_frames[XLANG_STACKTRACE_MAX_DEPTH];
static XLANG_THREAD_LOCAL x_i64 cached_size = -1;


static x_i64 capture_stacktrace(x_i64* dest, x_i64 max_depth, int skip)
{
    if (dest == NULL || max_depth <= 0)
        return 0;

#if defined(_WIN32)

    void* frames[XLANG_STACKTRACE_MAX_DEPTH];

    if (max_depth > XLANG_STACKTRACE_MAX_DEPTH)
        max_depth = XLANG_STACKTRACE_MAX_DEPTH;

    /*
        skip:
        0 = include CaptureStackBackTrace caller
        1 = skip capture_stacktrace
        2 = skip capture_stacktrace + public API wrapper
    */
    USHORT captured = CaptureStackBackTrace(
        (DWORD)(skip),
        (DWORD)(max_depth),
        frames,
        NULL
    );

    for (USHORT i = 0; i < captured; i++)
        dest[i] = (x_i64)(uintptr_t)frames[i];

    return (x_i64)(captured);

#elif defined(XLANG_HAS_EXECINFO)

    void* frames[XLANG_STACKTRACE_MAX_DEPTH];

    if (max_depth > XLANG_STACKTRACE_MAX_DEPTH)
        max_depth = XLANG_STACKTRACE_MAX_DEPTH;

    int captured = backtrace(frames, (int)(max_depth));

    if (captured <= skip)
        return 0;

    x_i64 n = 0;

    for (int i = skip; i < captured; i++)
        dest[n++] = (x_i64)(uintptr_t)frames[i];

    return n;

#else

    (void)(skip);
    return 0;

#endif
}


x_i64 get_stacktrace_size(void)
{
    cached_size = capture_stacktrace(cached_frames, XLANG_STACKTRACE_MAX_DEPTH, 2);
    return cached_size;
}



static void xlang_stacktrace_fallback_name(
    x_i64 address,
    char* dest,
    size_t dest_size
) {
    if (dest == NULL || dest_size == 0) {
        return;
    }

    snprintf(
        dest,
        dest_size,
        "0x%llx",
        (unsigned long long)(uintptr_t)address
    );
}

#if defined(_WIN32)

static void xlang_stacktrace_init_symbols(void)
{
    static volatile LONG initialized = 0;

    if (InterlockedCompareExchange(&initialized, 1, 0) == 0)
    {
        HANDLE process = GetCurrentProcess();
        SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES);

        if (!SymInitialize(process, NULL, TRUE))
            InterlockedExchange(&initialized, 0);
    }
}

#endif

static void xlang_stacktrace_resolve_name(x_i64 address, char* dest, size_t dest_size)
{
    if (dest == NULL || dest_size == 0)
        return;

    dest[0] = '\0';

#if defined(_WIN32)

    xlang_stacktrace_init_symbols();

    {
        HANDLE process = GetCurrentProcess();

        char buffer[sizeof(SYMBOL_INFO) + XLANG_STACKTRACE_NAME_MAX];
        SYMBOL_INFO* symbol = NULL;
        DWORD64 displacement = 0;

        memset(buffer, 0, sizeof(buffer));

        symbol = (SYMBOL_INFO*)buffer;
        symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
        symbol->MaxNameLen = XLANG_STACKTRACE_NAME_MAX - 1;

        if (SymFromAddr(
                process,
                (DWORD64)(uintptr_t)address,
                &displacement,
                symbol
            )) {
            if (symbol->Name != NULL && symbol->Name[0] != '\0') {
                snprintf(dest, dest_size, "%s", symbol->Name);
                return;
            }
        }
    }

    xlang_stacktrace_fallback_name(address, dest, dest_size);

#elif defined(XLANG_HAS_DLADDR)

    {
        Dl_info info;

        memset(&info, 0, sizeof(info));

        if (dladdr((void*)(uintptr_t)address, &info) != 0) {
            if (info.dli_sname != NULL && info.dli_sname[0] != '\0') {
                snprintf(dest, dest_size, "%s", info.dli_sname);
                return;
            }
        }
    }

    xlang_stacktrace_fallback_name(address, dest, dest_size);

#else

    xlang_stacktrace_fallback_name(address, dest, dest_size);

#endif
}


int get_stacktrace_name_len(x_i64 address)
{
    char temp[XLANG_STACKTRACE_NAME_MAX];

    xlang_stacktrace_resolve_name(
        address,
        temp,
        sizeof(temp)
    );

    return (int)strlen(temp);
}


void get_stacktrace_name(x_i64 address, char* dest)
{
    int len;

    if (dest == NULL) {
        return;
    }

    len = get_stacktrace_name_len(address);

    xlang_stacktrace_resolve_name(address, dest, (size_t)len + 1);
}