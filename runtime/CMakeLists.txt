# Copyright (c) 2026 Di Wang
# SPDX-License-Identifier: MIT
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

# This CMake file builds the native xlang runtime libraries.
#
# It is intended to be included from the root CMakeLists.txt through
# add_subdirectory(runtime). The file collects runtime source files from
# the core, exception, and garbage-collection runtime modules, then builds
# both the static and shared versions of the xlang core runtime library.
#
# Generated artifacts:
#
#   Static library:
#       build/xlang/libs/native/libxlang-core.a
#
#   Shared library:
#       build/xlang/runtime/native/libxlang-core.dll
#
#   Import library:
#       build/xlang/libs/native/libxlang-core.dll.a
#
#   Module definition file:
#       build/xlang/runtime/native/libxlang-core.def
#
# The .def file is generated only when building with MinGW.
# 
# Written by Di Wang
#

set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")
set(INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/include")

set(XLANG_CORE_SOURCE_DIR "${SOURCE_DIR}/core")
set(XLANG_CORE_INCLUDE_DIR "${INCLUDE_DIR}/core")
set(XLANG_CORE_RT_SOURCE_DIR "${XLANG_CORE_SOURCE_DIR}/xlang/runtime")

set(XLANG_EXCEPTION_SOURCE_DIR "${SOURCE_DIR}/exception")
set(XLANG_EXCEPTION_INCLUDE_DIR "${INCLUDE_DIR}/exception")
set(XLANG_EXCEPTION_RT_SOURCE_DIR "${XLANG_EXCEPTION_SOURCE_DIR}/xlang/runtime")

set(XLANG_GC_SOURCE_DIR "${SOURCE_DIR}/gc")
set(XLANG_GC_INCLUDE_DIR "${INCLUDE_DIR}/gc")
set(XLANG_GC_RT_SOURCE_DIR "${XLANG_GC_SOURCE_DIR}/xlang/runtime")

set(XLANG_NATIVE_LIB_DIR "${CMAKE_BINARY_DIR}/xlang/libs/native")
set(XLANG_NATIVE_RUNTIME_DIR "${CMAKE_BINARY_DIR}/xlang/runtime/native")


set(XLANG_CORE_SOURCES
    "${XLANG_CORE_RT_SOURCE_DIR}/io/console.c"
    "${XLANG_CORE_RT_SOURCE_DIR}/io/file.c"
    "${XLANG_CORE_RT_SOURCE_DIR}/memory/sysmem.c"
    "${XLANG_CORE_RT_SOURCE_DIR}/math.c"
    # "${XLANG_CORE_RT_SOURCE_DIR}/stacktrace.c"
    "${XLANG_CORE_RT_SOURCE_DIR}/system.c"

    "${XLANG_GC_RT_SOURCE_DIR}/memory/memlist.c"
    "${XLANG_GC_RT_SOURCE_DIR}/memory/memory.c"
)


# build static library: libxlang-core.a
add_library(xlang_core_static STATIC
    ${XLANG_CORE_SOURCES}
)

target_include_directories(xlang_core_static
    PUBLIC
    "${XLANG_CORE_INCLUDE_DIR}"
    "${XLANG_EXCEPTION_INCLUDE_DIR}"
    "${XLANG_GC_INCLUDE_DIR}"
)

set_target_properties(xlang_core_static PROPERTIES
    OUTPUT_NAME "xlang-core"
    PREFIX "lib"
    ARCHIVE_OUTPUT_DIRECTORY "${XLANG_NATIVE_LIB_DIR}"
)


# build shared library:
#   libxlang-core.dll
#   libxlang-core.dll.a
#   libxlang-core.def
add_library(xlang_core_shared SHARED
    ${XLANG_CORE_SOURCES}
)

target_include_directories(xlang_core_shared
    PUBLIC
    "${XLANG_CORE_INCLUDE_DIR}"
    "${XLANG_EXCEPTION_INCLUDE_DIR}"
    "${XLANG_GC_INCLUDE_DIR}"
)

set_target_properties(xlang_core_shared PROPERTIES
    OUTPUT_NAME "xlang-core"
    PREFIX "lib"

    # .dll.a import library
    ARCHIVE_OUTPUT_NAME "xlang-core"
    ARCHIVE_OUTPUT_DIRECTORY "${XLANG_NATIVE_LIB_DIR}"

    # .dll runtime binary
    RUNTIME_OUTPUT_NAME "xlang-core"
    RUNTIME_OUTPUT_DIRECTORY "${XLANG_NATIVE_RUNTIME_DIR}"

    WINDOWS_EXPORT_ALL_SYMBOLS ON
)


# generate .def file with MinGW
if (MINGW)
    target_link_options(xlang_core_shared
        PRIVATE
        "-Wl,--output-def,${XLANG_NATIVE_RUNTIME_DIR}/libxlang-core.def"
    )
endif()