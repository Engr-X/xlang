# xlang executable (can be passed from parent with -DXLANG=...).
if (NOT DEFINED XLANG OR XLANG STREQUAL "")
    find_program(XLANG NAMES xlang xlang.exe)
endif()

if (NOT XLANG)
    message(FATAL_ERROR "Cannot find xlang executable. Use -DXLANG=/path/to/xlang(.exe)")
endif()

# step 1: build codegen file


###############################################################

# step 2: build xlang sources

set(XLANG_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(XLANG_OBJ_ROOT "${CMAKE_BINARY_DIR}/objs/main")
set(XLANG_LIBS_DIR "${CMAKE_BINARY_DIR}/3rd-party/libs")
set(XLANG_LIBS
    "${XLANG_LIBS_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}cjson${CMAKE_STATIC_LIBRARY_SUFFIX}"
    "${XLANG_LIBS_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}tiny-regex${CMAKE_STATIC_LIBRARY_SUFFIX}"
)

collect_xlang_sources(XLANG_SOURCES "${XLANG_SOURCE_DIR}")

# filter xlang/compiler/codegen
list(FILTER XLANG_SOURCES EXCLUDE REGEX ".*[\\\\/]xlang[\\\\/]compiler[\\\\/]codegen[\\\\/].*")

set(XLANG_SRCS)
set(XLANG_OBJS)
set(XLANG_COMPILER_OBJS)
set(XLANG_OBJ_DIRS)

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

    if (src_rel MATCHES "^xlang/compiler/" OR src_rel MATCHES "^xlang/util/")
        list(APPEND XLANG_COMPILER_OBJS "${obj_abs}")
    endif()
endforeach()

list(REMOVE_DUPLICATES XLANG_OBJ_DIRS)

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
else()
    add_custom_target(xlang_objects
        COMMENT "No xlang source files found under ${XLANG_SOURCE_DIR}"
    )
endif()


###############################################################

# step 3: build xlang executable

set(XLANG_DIR "${CMAKE_BINARY_DIR}/xlang")
set(XLANG_EXE "${XLANG_DIR}/xlang${CMAKE_EXECUTABLE_SUFFIX}")

if (XLANG_SRCS)
    add_custom_command(
        OUTPUT "${XLANG_EXE}"

        COMMAND ${CMAKE_COMMAND} -E make_directory "${XLANG_DIR}"
        COMMAND ${XLANG}
            --target=x64
            -lib ${XLANG_COMPILER_OBJS} ${XLANG_LIBS}
            -d "${XLANG_EXE}"
            "--include-runtime"
            --main=xlang.compiler.Xlang.main

        DEPENDS ${XLANG_COMPILER_OBJS} cjson tiny-regex ${XLANG}

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
