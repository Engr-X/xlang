# xlang executable (can be passed from parent with -DXLANG=...).
if (NOT DEFINED XLANG OR XLANG STREQUAL "")
    find_program(XLANG NAMES xlang xlang.exe)
endif()

if (NOT XLANG)
    message(FATAL_ERROR "Cannot find xlang executable. Use -DXLANG=/path/to/xlang(.exe)")
endif()


# build codegen file



# Source root for package path resolution.
set(XLANG_SRC_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/main")

# Keep generated object files in build tree with the same relative layout.
set(XLANG_OBJ_ROOT "${CMAKE_BINARY_DIR}/objs")

# collect all xlang source files
file(GLOB_RECURSE XLANG_SRCS_ABS
    CONFIGURE_DEPENDS
    "${XLANG_SRC_ROOT}/*.x"
    "${XLANG_SRC_ROOT}/*.xl"
    "${XLANG_SRC_ROOT}/*.xlang"
)

# 2. build source list for xlang -c and object list for CMake dependency graph
set(XLANG_SRCS)
set(XLANG_OBJS)
set(XLANG_OBJ_DIRS)

foreach(src_abs ${XLANG_SRCS_ABS})
    file(RELATIVE_PATH src_rel "${XLANG_SRC_ROOT}" "${src_abs}")
    string(REPLACE "\\" "/" src_rel "${src_rel}")
    list(APPEND XLANG_SRCS "${src_rel}")

    get_filename_component(src_dir "${src_rel}" DIRECTORY)
    get_filename_component(src_name_we "${src_rel}" NAME_WE)

    if (src_dir STREQUAL "")
        set(obj_abs "${XLANG_OBJ_ROOT}/${src_name_we}.o")
    else()
        set(obj_abs "${XLANG_OBJ_ROOT}/${src_dir}/${src_name_we}.o")
    endif()

    list(APPEND XLANG_OBJS "${obj_abs}")
    get_filename_component(obj_dir "${obj_abs}" DIRECTORY)
    list(APPEND XLANG_OBJ_DIRS "${obj_dir}")
endforeach()

list(REMOVE_DUPLICATES XLANG_OBJ_DIRS)

if (XLANG_SRCS)
    add_custom_command(
        OUTPUT ${XLANG_OBJS}

        COMMAND ${CMAKE_COMMAND} -E make_directory ${XLANG_OBJ_DIRS}
        COMMAND ${XLANG}
            --target=x64
            --root=${XLANG_SRC_ROOT}
            -c ${XLANG_SRCS}
            -d ${XLANG_OBJ_ROOT}

        DEPENDS ${XLANG_SRCS_ABS}

        WORKING_DIRECTORY ${XLANG_SRC_ROOT}

        COMMENT "Compiling xlang sources"
        VERBATIM
        COMMAND_EXPAND_LISTS
    )

    add_custom_target(xlang_objects ALL
        DEPENDS ${XLANG_OBJS}
    )
else()
    add_custom_target(xlang_objects
        COMMENT "No xlang source files found under ${XLANG_SRC_ROOT}"
    )
endif()


# Export object list for parent CMakeLists if needed.
# set(XLANG_OBJS "${XLANG_OBJS}" PARENT_SCOPE)
