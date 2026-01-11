cmake_minimum_required(VERSION 4.1)
project(xlang LANGUAGES C)


set(HASKELL_DIR "${CMAKE_CURRENT_SOURCE_DIR}/haskell")
set(HASKELL_BUILD_DIR "${HASKELL_DIR}/build")

set(C_DIR "${CMAKE_CURRENT_SOURCE_DIR}/c")
set(C_BUILD_DIR "${C_DIR}/build")

file(MAKE_DIRECTORY ${HASKELL_BUILD_DIR})

# build Parser
add_subdirectory(${HASKELL_DIR} ${HASKELL_BUILD_DIR})


add_custom_target(
    xlang_parser_lib ALL
    DEPENDS ${STATIC_LIB}
)

set(PARSER_LIB "${HASKELL_BUILD_DIR}/libxlang_parser.a")
set(TARGET_DIR "${C_DIR}/lib")


file(MAKE_DIRECTORY ${TARGET_DIR})

add_custom_target(copy_haskell_lib ALL
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${PARSER_LIB}
            ${TARGET_DIR}/libxlang_parser.a
    DEPENDS xlang_parser_lib
    COMMENT "Copying Haskell static library to c/lib"
)

