cmake_minimum_required (VERSION 3.5)
message(STATUS "Fortran TESTING ON")
enable_language (C)
if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
    enable_language(Fortran)
endif()
enable_testing ()

set(FORTRAN_TEST_LIBRARIES ${YGG_TARGET_FORTRAN})
set(FORTRAN_TEST_INCLUDE_DIRS ${YGG_FORTRAN_MOD_DIR})

message(STATUS "FORTRAN_TEST_LIBRARIES = ${FORTRAN_TEST_LIBRARIES}")
message(STATUS "FORTRAN_TEST_INCLUDE_DIRS = ${FORTRAN_TEST_INCLUDE_DIRS}")

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

    # set(CMAKE_TESTDRIVER_BEFORE_TESTMAIN "ygg_init();")
    set(CMAKE_TESTDRIVER_AFTER_TESTMAIN "ygg_exit();")
    set(TARGET_SOURCE main.c)
    create_test_sourcelist (_ ${TARGET_SOURCE} ${TEST_FILES_MANGLED}
                            EXTRA_INCLUDE YggInterface.h)

    message(STATUS "FORTRAN_TEST_INCLUDE_DIRS = ${FORTRAN_TEST_INCLUDE_DIRS}")
    add_mixed_fortran_library(
        ${TARGET}_fortran SHARED
	LANGUAGE CXX ${TEST_FILES}
	LIBRARIES ${FORTRAN_TEST_LIBRARIES}
	INCLUDES ${FORTRAN_TEST_INCLUDE_DIRS}
	DEFINITIONS ${DYNAMIC_TEST_DEFINITIONS})
    # add_external_fortran_library(
    #     ${TARGET}_fortran SHARED SOURCES ${TEST_FILES}
    #     LIBRARIES ${FORTRAN_TEST_LIBRARIES}
    # 	INCLUDES ${FORTRAN_TEST_INCLUDE_DIRS})
    add_executable (${TARGET} ${TARGET_SOURCE})
    target_link_libraries (${TARGET} ${TARGET}_fortran ${FORTRAN_TEST_LIBRARIES})
    target_include_directories(${TARGET} PRIVATE ${FORTRAN_TEST_INCLUDE_DIRS})
    # ${CMAKE_CURRENT_SOURCE_DIR})

    set (TEST_TIMEOUT 40)
    # if (WIN32)
    #   set (TEST_TIMEOUT 100)
    # endif()
    set (INDEX 0)
    list (LENGTH TEST_FILES LEN)
    while (${LEN} GREATER ${INDEX})
        list (GET TEST_FILES ${INDEX} TEST_FILE)
	list (GET TEST_FILES_MANGLED ${INDEX} TEST_FILE_MANGLED)
	add_test (
	    NAME ${TEST_FILE}
	    COMMAND $<TARGET_FILE:${TARGET}> ${TEST_FILE_MANGLED})
	set_tests_properties (
	    ${TEST_FILE}
	    PROPERTIES TIMEOUT ${TEST_TIMEOUT}
	    ENVIRONMENT "${RAPIDJSON_PYTHON_ENV2}")
        math (EXPR INDEX "${INDEX} + 1")
    endwhile ()

    include(CheckDLL)
    show_runtimes(${TARGET})
    show_runtimes(${TARGET}_fortran AFTER_TARGET ${TARGET})
    show_symbols(${TARGET}_fortran AFTER_TARGET ${TARGET})
    if (WIN32)
        if (CONDA_PREFIX)
            add_custom_command(
                TARGET ${TARGET}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:YggInterface> $<TARGET_FILE_DIR:${TARGET}>
                COMMAND_EXPAND_LISTS
            )
            add_custom_command(
                TARGET ${TARGET}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:YggInterface_fortran> $<TARGET_FILE_DIR:${TARGET}>
                COMMAND_EXPAND_LISTS
            )
            add_custom_command(
                TARGET ${TARGET}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${TARGET}_fortran> $<TARGET_FILE_DIR:${TARGET}>
                COMMAND_EXPAND_LISTS
            )
        else()
            add_custom_command(
                TARGET ${TARGET}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_RUNTIME_DLLS:${TARGET}> $<TARGET_FILE_DIR:${TARGET}>
                COMMAND_EXPAND_LISTS
            )
        endif()
    endif ()

endfunction ()


file(GLOB files "*.f90")
message(STATUS "files = ${files}")
add_fortran_test_executable (fortran_testsuite ${files})
cover_test(fortran_testsuite)

include(DynamicTestLibraries)
add_dynamic_dependencies(fortran_testsuite ${DYNAMIC_TEST_LIBRARIES})
