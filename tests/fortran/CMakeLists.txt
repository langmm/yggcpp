cmake_minimum_required(VERSION 3.16)
message(STATUS "Fortran TESTING ON")
enable_language (C)
if ((NOT FORCE_SPLIT_CXXFortran) AND (NOT MSVC))
    enable_language(Fortran)
endif()
enable_testing ()
include(DynamicTestLibraries)

set(Fortran_TEST_LIBRARIES ${YGG_TARGET_Fortran})
set(Fortran_TEST_INCLUDE_DIRS ${YGG_Fortran_MOD_DIR})
set(WORKING_DIR "${CMAKE_CURRENT_BINARY_DIR}")
message(STATUS "WORKING_DIR = ${WORKING_DIR}")

function (mangle_fortran_name CNAME FNAME)
    set (TMP)
    # if (WIN32)
    #     string (TOUPPER "${FNAME}" TMP)
    # else ()
        string (TOLOWER "${FNAME}_" TMP)
    # endif ()
    set (${CNAME} ${TMP} PARENT_SCOPE)
endfunction ()


function (mangle_fortran_filename_list MANGLED)
    set (TMP)
    foreach (TFILE ${ARGN})
        cmake_path(GET TFILE FILENAME TFILE)
        string (REGEX REPLACE ".f90$" "" TESTNAME ${TFILE})
	mangle_fortran_name (C_TESTNAME ${TESTNAME})
	list (APPEND TMP ${C_TESTNAME})
    endforeach ()
    set (${MANGLED} ${TMP} PARENT_SCOPE)
endfunction()


function (add_fortran_test_executable TARGET)
    set (TEST_FILES ${ARGN})
    mangle_fortran_filename_list (TEST_FILES_MANGLED ${TEST_FILES})

    # set(CMAKE_TESTDRIVER_BEFORE_TESTMAIN "ygg_init();")
    set(CMAKE_TESTDRIVER_AFTER_TESTMAIN "ygg_exit();")
    set(TARGET_SOURCE main.c)
    create_test_sourcelist (_ ${TARGET_SOURCE} ${TEST_FILES_MANGLED}
                            EXTRA_INCLUDE YggInterface.h)

    add_mixed_language_library(
        ${TARGET}_fortran SHARED
        LINKER_LANGUAGE CXX
        SOURCES ${TEST_FILES}
	LIBRARIES ${Fortran_TEST_LIBRARIES}
	INCLUDES ${Fortran_TEST_INCLUDE_DIRS}
	DEFINITIONS ${DYNAMIC_TEST_DEFINITIONS}
    )
    # add_mixed_fortran_library(
    #     ${TARGET}_fortran SHARED
    #     LANGUAGE CXX ${TEST_FILES}
    #     LIBRARIES ${Fortran_TEST_LIBRARIES}
    #     INCLUDES ${Fortran_TEST_INCLUDE_DIRS}
    #     DEFINITIONS ${DYNAMIC_TEST_DEFINITIONS})
    add_executable (${TARGET} ${TARGET_SOURCE})
    target_link_libraries (${TARGET} ${TARGET}_fortran ${Fortran_TEST_LIBRARIES})
    target_include_directories(${TARGET} PRIVATE ${Fortran_TEST_INCLUDE_DIRS})

    set (TEST_TIMEOUT 40)
    set (INDEX 0)
    list (LENGTH TEST_FILES LEN)
    while (${LEN} GREATER ${INDEX})
        list (GET TEST_FILES ${INDEX} TEST_FILE)
	list (GET TEST_FILES_MANGLED ${INDEX} TEST_FILE_MANGLED)
	add_test (
	    NAME ${TEST_FILE}
	    COMMAND $<TARGET_FILE:${TARGET}> ${TEST_FILE_MANGLED}
	    WORKING_DIRECTORY ${WORKING_DIR})
        math (EXPR INDEX "${INDEX} + 1")
    endwhile ()
    set_tests_properties(
      ${TEST_FILES} PROPERTIES TIMEOUT ${TEST_TIMEOUT}
    )
    set_ygg_test_env(${TEST_FILES})
    # set_ygg_test_env(${TEST_FILES} OUTPUT_PROPERTIES ftest_properties)
    # list(LENGTH ftest_properties ftest_properties_LEN)
    # message(FATAL_ERROR "ftest_properties = ${ftest_properties}, LEN=${ftest_properties_LEN}")

    include(CheckDLL)
    show_runtimes(${TARGET})
    show_runtimes(${TARGET}_fortran AFTER_TARGET ${TARGET})
    show_symbols(${TARGET}_fortran AFTER_TARGET ${TARGET})
    copy_required_runtimes(
      ${TARGET} DEPENDENCIES
      YggInterface YggInterface_fortran ${TARGET}_fortran
    )

endfunction ()


file(GLOB files "*.f90")
generate_dynamic_tests(
  test_ygg_function_language_1.f90.in APPEND_TO files
)
add_fortran_test_executable (fortran_testsuite ${files})
cover_test(fortran_testsuite)

add_dynamic_dependencies(fortran_testsuite WORKING_DIR ${WORKING_DIR})
