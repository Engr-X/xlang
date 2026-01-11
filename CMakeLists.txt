cmake_minimum_required(VERSION 4.1)
project(xlang LANGUAGES C)


set(LEXPARSE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/lexparse")
set(LEXPARSE_BUILD_DIR "${LEXPARSE_DIR}/build")
set(LEXPARSE_BUILD_LIB_DIR "${LEXPARSE_BUILD_DIR}/lib")

set(C_DIR "${CMAKE_CURRENT_SOURCE_DIR}/c")
set(C_BUILD_DIR "${C_DIR}/build")

# build haskell

file(MAKE_DIRECTORY ${LEXPARSE_BUILD_LIB_DIR})

add_subdirectory(${LEXPARSE_DIR} ${LEXPARSE_BUILD_DIR})

add_custom_target(
    parser_lib ALL
)

# build C

# file(MAKE_DIRECTORY ${C_BUILD_DIR})
# file(MAKE_DIRECTORY ${C_BUILD_LIB_DIR})

# add_subdirectory(${C_DIR} ${C_BUILD_DIR})

# add_custom_target(
#    c_lib ALL
#)


