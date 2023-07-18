enable_testing()

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")
find_package(GTestSrc)

find_package(cmocka)
if(cmocka_FOUND)
    message(STATUS "Found CMOCKA")
else()
    include(FetchContent)
    FetchContent_Declare(
            cmocka_ygg
            GIT_REPOSITORY https://gitlab.com/cmocka/cmocka.git
            GIT_TAG f5e2cd77c88d9f792562888d2b70c5a396bfbf7a
    )
    FetchContent_MakeAvailable(cmocka_ygg)
endif()

if(GTESTSRC_FOUND)

    if (WIN32 AND (NOT CYGWIN) AND (NOT MINGW))
        set(gtest_disable_pthreads ON)
        set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    endif()

    IF(GTEST_SOURCE_DIR)
        add_subdirectory(${GTEST_SOURCE_DIR} ${CMAKE_BINARY_DIR}/googletest)
    endif()
    include_directories(SYSTEM ${GTEST_INCLUDE_DIR})

    set(TEST_LIBRARIES gtest gtest_main)
endif()

set(TEST_LIBRARIES ${TEST_LIBRARIES} YggInterface ${DEPS_LIB_NAMES} ${DEPS_LIBRARIES})
set(TEST_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS})

option(YGG_ENABLE_ELF "Enable ELF replacements when available" ON)
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND YGG_ENABLE_ELF)
   add_definitions(-DELF_AVAILABLE)
   add_definitions(-DSUBLIBFILE="${SUBLIBFILE}")
   set(TEST_LIBRARIES ${TEST_LIBRARIES} dl)
endif()

if(GTESTSRC_FOUND)

    include(unittest/CMakeLists.txt)
    add_definitions(-DYGG_TEST)

    if(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
        find_program(GCOVR_FOUND gcovr)
        if(GCOVR_FOUND)
            message(STATUS "gcovr found")
            file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/coverage)
            add_test(NAME coverage
                     COMMAND sh -c "gcovr ${CMAKE_BINARY_DIR} --gcov-executable=\"${CTEST_COVERAGE_COMMAND} ${CTEST_COVERAGE_EXTRA_FLAGS}\" --coveralls ${CMAKE_BINARY_DIR}/coverage/coverage.info --html-details ${CMAKE_BINARY_DIR}/coverage/coverage.html"
                     WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
            set_tests_properties(coverage PROPERTIES DEPENDS "unittest")
        endif(GCOVR_FOUND)
    endif(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
endif()

