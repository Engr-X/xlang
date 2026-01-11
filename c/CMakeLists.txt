cmake_minimum_required(VERSION 4.1)
project(xlang_c LANGUAGES C)


set(C_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")
set(C_BUILD_DIR "${CMAKE_CURRENT_SOURCE_DIR}/build")
set(C_BUILD_LIB_DIR "${C_BUILD_DIR}/lib")

file(MAKE_DIRECTORY ${C_BUILD_LIB_DIR})

include_directories(
    ${CMAKE_SOURCE_DIR}/include
)

add_library(xlang_c STATIC
    ${C_SRC_DIR}/xlang.c
)

target_include_directories(xlang_c PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

set_target_properties(xlang_c PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${C_BUILD_LIB_DIR}
)

set_target_properties(xlang_c PROPERTIES
    OUTPUT_NAME "xlang_c"
)
