cmake_minimum_required (VERSION 3.5)
enable_language (C Fortran)
enable_testing ()

# TODO: Determine where this path should really come from
cmake_path(GET CMAKE_CURRENT_BINARY_DIR PARENT_PATH MOD_PATH)
message(STATUS "MOD_PATH = ${MOD_PATH}")

set(TEST_LIBRARIES YggInterface_fortran YggInterface ${DEPS_LIB_NAMES} ${DEPS_LIBRARIES})
set(TEST_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS} ${MOD_PATH})

message(STATUS "TEST_LIBRARIES = ${TEST_LIBRARIES}")
message(STATUS "TEST_INCLUDE_DIRS = ${TEST_INCLUDE_DIRS}")

function (mangle_fortran_name CNAME FNAME)
    set (TMP)
    if (WIN32)
        string (TOUPPER "${FNAME}" TMP)
    else ()
        string (TOLOWER "${FNAME}_" TMP)
    endif ()
    set (${CNAME} ${TMP} PARENT_SCOPE)
endfunction ()


function (mangle_fortran_filename_list MANGLED)
    set (TMP)
    foreach (TFILE ${ARGN})
        cmake_path(RELATIVE_PATH TFILE)
        string (REGEX REPLACE ".f90$" "" TESTNAME ${TFILE})
	mangle_fortran_name (C_TESTNAME ${TESTNAME})
	list (APPEND TMP ${C_TESTNAME})
    endforeach ()
    set (${MANGLED} ${TMP} PARENT_SCOPE)
endfunction()


function (add_fortran_test_executable TARGET)
    set (TEST_FILES ${ARGN})
    mangle_fortran_filename_list (TEST_FILES_MANGLED ${TEST_FILES})

    create_test_sourcelist (_ main.c ${TEST_FILES_MANGLED})

    add_library (${TARGET}_fortran ${TEST_FILES})
    target_link_libraries (${TARGET}_fortran PUBLIC ${TEST_LIBRARIES})
    target_include_directories(${TARGET}_fortran PUBLIC ${TEST_INCLUDE_DIRS})
    add_executable (${TARGET} main.c)
    target_link_libraries (${TARGET} ${TARGET}_fortran ${TEST_LIBRARIES})
    target_include_directories(${TARGET} PRIVATE ${TEST_INCLUDE_DIRS})

    set (INDEX 0)
    list (LENGTH TEST_FILES LEN)
    while (${LEN} GREATER ${INDEX})
        list (GET TEST_FILES ${INDEX} TEST_FILE)
	list (GET TEST_FILES_MANGLED ${INDEX} TEST_FILE_MANGLED)
	add_test (
	    NAME ${TEST_FILE}
	    COMMAND $<TARGET_FILE:${TARGET}> ${TEST_FILE_MANGLED})
        math (EXPR INDEX "${INDEX} + 1")
    endwhile ()
endfunction ()


file(GLOB files "*.f90")
add_fortran_test_executable (fortran_testsuite ${files})
