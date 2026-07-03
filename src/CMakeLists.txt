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

# xlang executable (can be passed from parent with -DXLANG=...).
if (NOT DEFINED XLANG OR XLANG STREQUAL "")
    find_program(XLANG NAMES xlang xlang.exe)
endif()

if (NOT XLANG)
    message(FATAL_ERROR "Cannot find xlang executable. Use -DXLANG=/path/to/xlang(.exe)")
endif()

if (NOT DEFINED PYTHON_EXECUTABLE OR PYTHON_EXECUTABLE STREQUAL "")
    find_program(PYTHON_EXECUTABLE NAMES python python3 python.exe)
endif()

if (NOT PYTHON_EXECUTABLE)
    message(FATAL_ERROR "Cannot find Python executable. Use -DPYTHON_EXECUTABLE=/path/to/python(.exe)")
endif()


function(collect_xlang_sources out_var root_dir)
    file(GLOB_RECURSE _xlang_sources
        CONFIGURE_DEPENDS
        "${root_dir}/*.x"
        "${root_dir}/*.xl"
        "${root_dir}/*.xlang"
    )

    set(${out_var} ${_xlang_sources} PARENT_SCOPE)
endfunction()


add_subdirectory(main)

set(XLANG_TEST_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/test")
collect_xlang_sources(XLANG_TEST_SOURCES "${XLANG_TEST_SOURCE_DIR}")

if (XLANG_TEST_SOURCES)
    add_subdirectory(test)
endif()
