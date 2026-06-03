# xlang executable (can be passed from parent with -DXLANG=...).
if (NOT DEFINED XLANG OR XLANG STREQUAL "")
    find_program(XLANG NAMES xlang xlang.exe)
endif()

if (NOT XLANG)
    message(FATAL_ERROR "Cannot find xlang executable. Use -DXLANG=/path/to/xlang(.exe)")
endif()

# step 1: build codegen file


###############################################################

# step 2: build xlang test sources

set(XLANG_TEST_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(XLANG_TEST_OBJ_ROOT "${CMAKE_BINARY_DIR}/objs/test")
set(XLANG_TEST_LIBS_DIR "${CMAKE_BINARY_DIR}/3rd-party/libs")
set(XLANG_TEST_LIBS
    "${XLANG_TEST_LIBS_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}cjson${CMAKE_STATIC_LIBRARY_SUFFIX}"
    "${XLANG_TEST_LIBS_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}tiny-regex${CMAKE_STATIC_LIBRARY_SUFFIX}"
)

collect_xlang_sources(XLANG_TEST_SOURCES "${XLANG_TEST_SOURCE_DIR}")
get_property(XLANG_COMPILER_OBJS GLOBAL PROPERTY GP_XLANG_COMPILER_OBJS)

set(XLANG_TEST_SRCS)
set(XLANG_TEST_OBJS)
set(XLANG_TEST_OBJ_DIRS)

foreach(src_abs ${XLANG_TEST_SOURCES})
    file(RELATIVE_PATH src_rel "${XLANG_TEST_SOURCE_DIR}" "${src_abs}")
    string(REPLACE "\\" "/" src_rel "${src_rel}")
    list(APPEND XLANG_TEST_SRCS "${src_rel}")

    get_filename_component(src_dir "${src_rel}" DIRECTORY)
    get_filename_component(src_name_we "${src_rel}" NAME_WE)

    if (src_dir STREQUAL "")
        set(obj_dir "${XLANG_TEST_OBJ_ROOT}")
        set(obj_abs "${XLANG_TEST_OBJ_ROOT}/${src_name_we}.o")
    else()
        set(obj_dir "${XLANG_TEST_OBJ_ROOT}/${src_dir}")
        set(obj_abs "${XLANG_TEST_OBJ_ROOT}/${src_dir}/${src_name_we}.o")
    endif()

    list(APPEND XLANG_TEST_OBJ_DIRS "${obj_dir}")
    list(APPEND XLANG_TEST_OBJS "${obj_abs}")
endforeach()

list(REMOVE_DUPLICATES XLANG_TEST_OBJ_DIRS)

if (XLANG_TEST_SRCS)
    add_custom_command(
        OUTPUT ${XLANG_TEST_OBJS}

        COMMAND ${CMAKE_COMMAND} -E remove_directory "${XLANG_TEST_OBJ_ROOT}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${XLANG_TEST_OBJ_ROOT}" ${XLANG_TEST_OBJ_DIRS}
        COMMAND ${XLANG}
            --target=x64
            --root=${XLANG_TEST_SOURCE_DIR}
            -c ${XLANG_TEST_SRCS}
            -lib ${XLANG_COMPILER_OBJS}
            -d ${XLANG_TEST_OBJ_ROOT}

        DEPENDS ${XLANG_COMPILER_OBJS} cjson tiny-regex ${XLANG}

        WORKING_DIRECTORY ${XLANG_TEST_SOURCE_DIR}

        COMMENT "Compiling xlang test sources"
        VERBATIM
        COMMAND_EXPAND_LISTS
    )

    add_custom_target(xlang_test_objects ALL
        DEPENDS ${XLANG_TEST_OBJS}
    )
else()
    add_custom_target(xlang_test_objects
        COMMENT "No xlang test source files found under ${XLANG_TEST_SOURCE_DIR}"
    )
endif()


###############################################################

# step 3: build xlang test executable

set(XLANG_TEST_DIR "${CMAKE_BINARY_DIR}/xlang")
set(XLANG_TEST_EXE "${XLANG_TEST_DIR}/xlang-test${CMAKE_EXECUTABLE_SUFFIX}")
set(XLANG_TEST_MAIN "xlang.Test.main")

if (XLANG_TEST_SRCS)
    add_custom_command(
        OUTPUT "${XLANG_TEST_EXE}"

        COMMAND ${CMAKE_COMMAND} -E make_directory "${XLANG_TEST_DIR}"
        COMMAND ${XLANG}
            --target=x64
            -lib ${XLANG_COMPILER_OBJS} ${XLANG_TEST_OBJS} ${XLANG_TEST_LIBS}
            -d "${XLANG_TEST_EXE}"
            "--include-runtime"
            "--main=${XLANG_TEST_MAIN}"

        DEPENDS ${XLANG_COMPILER_OBJS} ${XLANG_TEST_OBJS} cjson tiny-regex ${XLANG}

        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}

        COMMENT "Linking xlang-test executable"
        VERBATIM
        COMMAND_EXPAND_LISTS
    )

    add_custom_target(xlang_test_executable
        DEPENDS "${XLANG_TEST_EXE}"
    )
else()
    add_custom_target(xlang_test_executable
        COMMENT "No xlang test object files available to build executable"
    )
endif()
