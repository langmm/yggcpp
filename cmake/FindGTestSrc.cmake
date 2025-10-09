
SET(GTEST_SEARCH_PATH
    "${GTEST_SOURCE_DIR}"
    "${CMAKE_CURRENT_LIST_DIR}/../thirdparty/gtest")

IF(UNIX)
    IF(RAPIDJSON_BUILD_THIRDPARTY_GTEST)
        LIST(APPEND GTEST_SEARCH_PATH "/usr/src/gtest")
    ELSE()
        LIST(INSERT GTEST_SEARCH_PATH 1 "/usr/src/gtest")
    ENDIF()
ENDIF()

FIND_PATH(GTEST_SOURCE_DIR
        NAMES src/gtest_main.cc
	PATH_SUFFIXES googletest
        PATHS ${GTEST_SEARCH_PATH})
        
string(FIND ${GTEST_SOURCE_DIR} "thirdparty" IDX_THIRDPARTY)
set(GTEST_THIRDPARTY OFF)
if(NOT IDX_THIRDPARTY EQUAL -1)
  set(GTEST_THIRDPARTY ON)
endif()
if(GTEST_THIRDPARTY)
  execute_process(
    COMMAND git apply ${CMAKE_CURRENT_LIST_DIR}/gtest.patch
    WORKING_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/../cpp/include/rapidjson/thirdparty/gtest"
    RESULT_VARIABLE GTEST_THIRDPARTY_PATCH_RESULT
  )
endif()

if(GTEST_SOURCE_DIR EQUAL GTEST_SOURCE_DIR-NOTFOUND)
    set(GTEST_SOURCE_DIR "")
endif()
	
FIND_FILE(GTEST_LIBRARY
        NAMES libgtest.a
	PATH_SUFFIXES lib
	PATHS ${GTEST_SEARCH_PATH})
if(${GTEST_LIBRARY} EQUAL GTEST_LIBRARY-NOTFOUND)
    set(GTEST_LIBRARY "")
else()
    cmake_path(REPLACE_FILENAME GTEST_LIBRARY libgtest_main.a OUTPUT_VARIABLE GTEST_LIBRARY_MAIN)
    set(GTEST_LIBRARIES ${GTEST_LIBRARY} ${GTEST_LIBRARY_MAIN})
endif()

if(GTEST_LIBRARY)
    set(GTEST_INCLUDE_HINT ${GTEST_LIBRARY})
    set(GTEST_SOURCE_DIR "")
else()
    set(GTEST_INCLUDE_HINT ${GTEST_SOURCE_DIR})
    set(GTEST_LIBRARY "")
    set(GTEST_LIBRARIES "")
endif()

# Debian installs gtest include directory in /usr/include, thus need to look
# for include directory separately from source directory.
FIND_PATH(GTEST_INCLUDE_DIR
        NAMES gtest/gtest.h
        PATH_SUFFIXES include
        HINTS ${GTEST_INCLUDE_HINT}
        PATHS ${GTEST_SEARCH_PATH})

INCLUDE(FindPackageHandleStandardArgs)
if(GTEST_SOURCE_DIR)
    find_package_handle_standard_args(GTestSrc DEFAULT_MSG
        GTEST_SOURCE_DIR
        GTEST_INCLUDE_DIR)
else()
    find_package_handle_standard_args(GTestSrc DEFAULT_MSG
        GTEST_LIBRARY
        GTEST_INCLUDE_DIR)
endif()
