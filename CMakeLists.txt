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

cmake_minimum_required(VERSION 3.15)

project(xlang C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

if (CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(-O3)
elseif (MSVC)
    add_compile_options(/O2)
endif()


# Required bootstrap tools.
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

message(STATUS "XLANG = ${XLANG}")
message(STATUS "PYTHON_EXECUTABLE = ${PYTHON_EXECUTABLE}")


# step 1: build 3rd-party dependencies
add_subdirectory(3rd-party)

# step 2: build xlang-runtime-lib
add_subdirectory(runtime)

add_subdirectory(src)
