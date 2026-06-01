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

# 
# Written by Di Wang
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


# step1: build xlang-runtime-lib
add_subdirectory(runtime)

add_subdirectory(src)
