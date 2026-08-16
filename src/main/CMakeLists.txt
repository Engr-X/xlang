# ===============================================================
# Step 0: locate the bootstrap xlang compiler
#
# This CMake file does not build the bootstrap compiler itself. It expects an
# existing xlang executable, either supplied by -DXLANG=... or discoverable from
# PATH. That executable is then used to compile the xlang sources in this tree.
# ===============================================================

if (NOT DEFINED XLANG OR XLANG STREQUAL "")
    find_program(XLANG NAMES xlang xlang.exe)
endif()

if (NOT XLANG)
    message(FATAL_ERROR "Cannot find xlang executable. Use -DXLANG=/path/to/xlang(.exe)")
endif()


# ===============================================================
# Step 1: generate compiler sources from JSON rule files
#
# The tokenizer and token normalizer contain large tables that are
# easier to maintain as JSON. These custom commands translate those JSON files
# into xlang source files before the main xlang compilation step runs.
# ===============================================================


set(XLANG_LEXER_CODEGEN_DIR "${CMAKE_CURRENT_SOURCE_DIR}/xlang/lexer/codegen")
set(XLANG_TOKENIZER_CODEGEN "${XLANG_LEXER_CODEGEN_DIR}/tokenizer_codegen.py")
set(XLANG_NORMALIZER_CODEGEN "${XLANG_LEXER_CODEGEN_DIR}/normalizer_codegen.py")
set(XLANG_LEXER_CODEGEN_UTILS
    "${XLANG_LEXER_CODEGEN_DIR}/__init__.py"
    "${XLANG_LEXER_CODEGEN_DIR}/util.py"
)
set(XLANG_COMPILER_TOKENIZER_RULES "${CMAKE_CURRENT_SOURCE_DIR}/resources/syntax/compiler_tokenizer.json")
set(XLANG_COMPILER_TOKEN_NORMALIZER_RULES "${CMAKE_CURRENT_SOURCE_DIR}/resources/syntax/compiler_token_normalizer.json")
set(XLANG_COMPILER_TOKENIZER "${CMAKE_CURRENT_SOURCE_DIR}/xlang/compiler/lexer/Tokenizer.x")
set(XLANG_COMPILER_TOKEN_NORMALIZER "${CMAKE_CURRENT_SOURCE_DIR}/xlang/compiler/lexer/TokenNormalizer.x")


# 1.1 Generate xlang.compiler.lexer.Tokenizer from compiler_tokenizer.json.
add_custom_command(
    OUTPUT "${XLANG_COMPILER_TOKENIZER}"

    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_SOURCE_DIR}/xlang/compiler/lexer"
    COMMAND ${PYTHON_EXECUTABLE}
        "${XLANG_TOKENIZER_CODEGEN}"
        -c "${XLANG_COMPILER_TOKENIZER_RULES}"
        -d "${XLANG_COMPILER_TOKENIZER}"

    DEPENDS
        "${XLANG_TOKENIZER_CODEGEN}"
        ${XLANG_LEXER_CODEGEN_UTILS}
        "${XLANG_COMPILER_TOKENIZER_RULES}"

    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"

    COMMENT "Generating compiler tokenizer"
    VERBATIM
)

add_custom_target(xlang_compiler_tokenizer_codegen
    DEPENDS "${XLANG_COMPILER_TOKENIZER}"
)


# 1.2 Generate xlang.compiler.lexer.TokenNormalizer.
#
# The normalizer generator also reads tokenizer rules so it can reuse token
# kinds, symbol definitions and token-pattern expansion.
add_custom_command(
    OUTPUT "${XLANG_COMPILER_TOKEN_NORMALIZER}"

    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_SOURCE_DIR}/xlang/compiler/lexer"
    COMMAND ${PYTHON_EXECUTABLE}
        "${XLANG_NORMALIZER_CODEGEN}"
        -c "${XLANG_COMPILER_TOKEN_NORMALIZER_RULES}"
        -t "${XLANG_COMPILER_TOKENIZER_RULES}"
        -d "${XLANG_COMPILER_TOKEN_NORMALIZER}"

    DEPENDS
        "${XLANG_NORMALIZER_CODEGEN}"
        "${XLANG_TOKENIZER_CODEGEN}"
        ${XLANG_LEXER_CODEGEN_UTILS}
        "${XLANG_COMPILER_TOKENIZER_RULES}"
        "${XLANG_COMPILER_TOKEN_NORMALIZER_RULES}"

    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"

    COMMENT "Generating compiler token normalizer"
    VERBATIM
)

add_custom_target(xlang_compiler_token_normalizer_codegen
    DEPENDS "${XLANG_COMPILER_TOKEN_NORMALIZER}"
)

###############################################################

# ===============================================================
# Step 2: compile project-local C support sources
#
# These C files are not part of the core runtime target. They are small native
# helpers used by the compiler layer, so they are compiled into standalone .o
# files and later linked together with xlang-generated objects.
# ===============================================================


set(XLANG_C_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/c/src")
set(XLANG_C_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/c/include")
set(XLANG_TINY_REGEX_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/3rd-party/tiny-regex/include")
set(XLANG_RUNTIME_CORE_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/runtime/include/core")
set(XLANG_C_OBJ_ROOT "${CMAKE_BINARY_DIR}/objs/src/main/c")

file(GLOB_RECURSE XLANG_C_SOURCES
    CONFIGURE_DEPENDS
    "${XLANG_C_SOURCE_DIR}/*.c"
)

set(XLANG_C_OBJS)
set(XLANG_C_OBJ_DIRS)

# First pass: calculate every C object path and every directory that must exist.
# The object layout mirrors the source tree under build/objs/src/main/c.
foreach(c_src_abs ${XLANG_C_SOURCES})
    file(RELATIVE_PATH c_src_rel "${XLANG_C_SOURCE_DIR}" "${c_src_abs}")
    string(REPLACE "\\" "/" c_src_rel "${c_src_rel}")

    get_filename_component(c_src_dir "${c_src_rel}" DIRECTORY)
    get_filename_component(c_src_name_we "${c_src_rel}" NAME_WE)

    if (c_src_dir STREQUAL "")
        set(c_obj_dir "${XLANG_C_OBJ_ROOT}")
        set(c_obj_abs "${XLANG_C_OBJ_ROOT}/${c_src_name_we}.o")
    else()
        set(c_obj_dir "${XLANG_C_OBJ_ROOT}/${c_src_dir}")
        set(c_obj_abs "${XLANG_C_OBJ_ROOT}/${c_src_dir}/${c_src_name_we}.o")
    endif()

    list(APPEND XLANG_C_OBJ_DIRS "${c_obj_dir}")
    list(APPEND XLANG_C_OBJS "${c_obj_abs}")
endforeach()

list(REMOVE_DUPLICATES XLANG_C_OBJ_DIRS)

# Tests need the same C object list when they link xlang-test, so expose it as a
# global property after it has been calculated.
set_property(GLOBAL PROPERTY GP_XLANG_C_OBJS "${XLANG_C_OBJS}")

if (XLANG_C_SOURCES)
    # Second pass: attach one custom command to each C object path.
    #
    # This intentionally mirrors the first pass instead of deriving paths from
    # a different helper, so CMake knows each generated .o file explicitly.
    foreach(c_src_abs ${XLANG_C_SOURCES})
        file(RELATIVE_PATH c_src_rel "${XLANG_C_SOURCE_DIR}" "${c_src_abs}")
        string(REPLACE "\\" "/" c_src_rel "${c_src_rel}")

        get_filename_component(c_src_dir "${c_src_rel}" DIRECTORY)
        get_filename_component(c_src_name_we "${c_src_rel}" NAME_WE)

        if (c_src_dir STREQUAL "")
            set(c_obj_dir "${XLANG_C_OBJ_ROOT}")
            set(c_obj_abs "${XLANG_C_OBJ_ROOT}/${c_src_name_we}.o")
        else()
            set(c_obj_dir "${XLANG_C_OBJ_ROOT}/${c_src_dir}")
            set(c_obj_abs "${XLANG_C_OBJ_ROOT}/${c_src_dir}/${c_src_name_we}.o")
        endif()

        add_custom_command(
            OUTPUT "${c_obj_abs}"

            COMMAND ${CMAKE_COMMAND} -E make_directory "${c_obj_dir}"
            COMMAND ${CMAKE_C_COMPILER}
                -I "${XLANG_C_INCLUDE_DIR}"
                -I "${XLANG_TINY_REGEX_INCLUDE_DIR}"
                -I "${XLANG_RUNTIME_CORE_INCLUDE_DIR}"
                -c "${c_src_abs}"
                -o "${c_obj_abs}"

            DEPENDS "${c_src_abs}"

            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}

            COMMENT "Compiling C source ${c_src_rel}"
            VERBATIM
            COMMAND_EXPAND_LISTS
        )
    endforeach()

    add_custom_target(xlang_c_objects ALL
        DEPENDS ${XLANG_C_OBJS}
    )
else()
    add_custom_target(xlang_c_objects
        COMMENT "No C source files found under ${XLANG_C_SOURCE_DIR}"
    )
endif()


###############################################################

# ===============================================================
# Step 3: compile all main xlang sources into object files
#
# This step compiles the standard library pieces in src/main/xlang, the compiler
# source, and all generated compiler files from Step 1. The resulting object
# list has two uses:
#   - XLANG_OBJS: every object from src/main, used by the build graph.
#   - XLANG_COMPILER_OBJS: the subset needed to link the self-hosted compiler
#     executable and tests.
# ===============================================================


set(XLANG_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(XLANG_OBJ_ROOT "${CMAKE_BINARY_DIR}/objs/main")
set(XLANG_LIBS_DIR "${CMAKE_BINARY_DIR}/3rd-party/libs")
set(XLANG_LIBS
    "${XLANG_LIBS_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}cjson${CMAKE_STATIC_LIBRARY_SUFFIX}"
    "${XLANG_LIBS_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}tiny-regex${CMAKE_STATIC_LIBRARY_SUFFIX}"
)

collect_xlang_sources(XLANG_SOURCES "${XLANG_SOURCE_DIR}")

# Generated files may not exist before configure, so append their expected paths
# explicitly after source collection.
if (DEFINED XLANG_COMPILER_TOKENIZER)
    list(APPEND XLANG_SOURCES "${XLANG_COMPILER_TOKENIZER}")
    list(REMOVE_DUPLICATES XLANG_SOURCES)
endif()

if (DEFINED XLANG_COMPILER_TOKEN_NORMALIZER)
    list(APPEND XLANG_SOURCES "${XLANG_COMPILER_TOKEN_NORMALIZER}")
    list(REMOVE_DUPLICATES XLANG_SOURCES)
endif()

# Python codegen sources are build tools, not xlang sources. Older generated
# lexer code is also excluded from the main compilation set.
list(FILTER XLANG_SOURCES EXCLUDE REGEX ".*[\\\\/]xlang[\\\\/]compiler[\\\\/]codegen[\\\\/].*")
list(FILTER XLANG_SOURCES EXCLUDE REGEX ".*[\\/]xlang[\\/]lex[\\/]Codegen\\.x$")

set(XLANG_SRCS)
set(XLANG_OBJS)
set(XLANG_COMPILER_OBJS)
set(XLANG_OBJ_DIRS)

# Map every xlang source to its generated object path. At the same time, build
# XLANG_COMPILER_OBJS: only the objects needed by the compiler executable and
# test executable are exported through the global property below.
foreach(src_abs ${XLANG_SOURCES})
    file(RELATIVE_PATH src_rel "${XLANG_SOURCE_DIR}" "${src_abs}")
    string(REPLACE "\\" "/" src_rel "${src_rel}")
    list(APPEND XLANG_SRCS "${src_rel}")

    get_filename_component(src_dir "${src_rel}" DIRECTORY)
    get_filename_component(src_name_we "${src_rel}" NAME_WE)

    if (src_dir STREQUAL "")
        set(obj_dir "${XLANG_OBJ_ROOT}")
        set(obj_abs "${XLANG_OBJ_ROOT}/${src_name_we}.o")
    else()
        set(obj_dir "${XLANG_OBJ_ROOT}/${src_dir}")
        set(obj_abs "${XLANG_OBJ_ROOT}/${src_dir}/${src_name_we}.o")
    endif()

    list(APPEND XLANG_OBJ_DIRS "${obj_dir}")
    list(APPEND XLANG_OBJS "${obj_abs}")

    if (src_rel MATCHES "^xlang/Diagnostic\\.x$" OR src_rel MATCHES "^xlang/compiler/" OR src_rel MATCHES "^xlang/lexer/" OR src_rel MATCHES "^xlang/parser/Parsers?\\.x$" OR src_rel MATCHES "^xlang/parser/RecursiveParser\\.x$" OR src_rel MATCHES "^xlang/parser/ParserUtil\\.x$" OR src_rel MATCHES "^xlang/util/")
        list(APPEND XLANG_COMPILER_OBJS "${obj_abs}")
    endif()
endforeach()

list(REMOVE_DUPLICATES XLANG_OBJ_DIRS)

# src/test/CMakeLists.txt reads this property to link xlang-test against the
# compiler/library objects compiled here.
set_property(GLOBAL PROPERTY GP_XLANG_COMPILER_OBJS "${XLANG_COMPILER_OBJS}")

if (XLANG_SRCS)
    add_custom_command(
        OUTPUT ${XLANG_OBJS}

        COMMAND ${CMAKE_COMMAND} -E remove_directory "${XLANG_OBJ_ROOT}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${XLANG_OBJ_ROOT}" ${XLANG_OBJ_DIRS}
        COMMAND ${XLANG}
            --target=x64
            --root=${XLANG_SOURCE_DIR}
            -c ${XLANG_SRCS}
            -d ${XLANG_OBJ_ROOT}

        DEPENDS ${XLANG_SOURCES} ${XLANG}

        WORKING_DIRECTORY ${XLANG_SOURCE_DIR}

        COMMENT "Compiling xlang sources"
        VERBATIM
        COMMAND_EXPAND_LISTS
    )

    add_custom_target(xlang_objects ALL
        DEPENDS ${XLANG_OBJS}
    )
    add_dependencies(xlang_objects
        xlang_compiler_tokenizer_codegen
        xlang_compiler_token_normalizer_codegen
    )
else()
    add_custom_target(xlang_objects
        COMMENT "No xlang source files found under ${XLANG_SOURCE_DIR}"
    )
endif()


###############################################################

# ===============================================================
# Step 4: link the self-hosted xlang compiler executable
#
# This links the generated compiler objects, project-local C helpers, third-party
# static libraries and the runtime into build/xlang/xlang(.exe).
# ===============================================================


set(XLANG_DIR "${CMAKE_BINARY_DIR}/xlang")
set(XLANG_EXE "${XLANG_DIR}/xlang${CMAKE_EXECUTABLE_SUFFIX}")

if (XLANG_SRCS)
    add_custom_command(
        OUTPUT "${XLANG_EXE}"

        COMMAND ${CMAKE_COMMAND} -E make_directory "${XLANG_DIR}"
        COMMAND ${XLANG}
            --target=x64
            -lib ${XLANG_COMPILER_OBJS} ${XLANG_C_OBJS} ${XLANG_LIBS}
            -d "${XLANG_EXE}"
            "--include-runtime"
            --main=xlang.compiler.Xlang.main

        DEPENDS ${XLANG_COMPILER_OBJS} ${XLANG_C_OBJS} cjson tiny-regex ${XLANG}

        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}

        COMMENT "Linking xlang executable"
        VERBATIM
        COMMAND_EXPAND_LISTS
    )

    add_custom_target(xlang_executable ALL
        DEPENDS "${XLANG_EXE}"
    )
else()
    add_custom_target(xlang_executable
        COMMENT "No xlang object files available to build executable"
    )
endif()
