include(CheckCXXCompilerFlag)

find_program(CCACH_FOUND ccache)
if(CCACHE_FOUND)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
    if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Qunused-arguments -fcolor-diagnostics")
    endif()
endif(CCACHE_FOUND)

set_property(DIRECTORY PROPERTY COMPILE_OPTIONS ${EXTRA_CXX_FLAGS})
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    # If the user is running a newer version of Clang that includes the
    # -Wdouble-promotion, we will ignore that warning.
    if (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER 3.7)
        CHECK_CXX_COMPILER_FLAG("-Wno-double-promotion" HAS_NO_DOUBLE_PROMOTION)
        if (HAS_NO_DOUBLE_PROMOTION)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-double-promotion")
        endif()
    endif()
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    # Force to always compile with /W4
    if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
        string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
    endif()

    # Force to always compile with /WX
    if(CMAKE_CXX_FLAGS MATCHES "/WX-")
        string(REGEX REPLACE "/WX-" "/WX" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /WX")
    endif()
endif()

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DRAPIDJSON_HAS_STDSTRING=1")

include(GoogleTest)
file(GLOB files "${CMAKE_CURRENT_SOURCE_DIR}/unittest/tests/communicators/*.cpp" "${CMAKE_CURRENT_SOURCE_DIR}/unittest/tests/utils/*.cpp")

include_directories(${TEST_INCLUDE_DIRS})
if(CONDA_PREFIX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I${CONDA_INCLUDE}")
    include_directories(SYSTEM ${CONDA_INCLUDE})
endif()
add_executable(unittest ${CMAKE_CURRENT_SOURCE_DIR}/unittest/unittest.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/elf_hook.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/mock.cpp ${files})
target_link_libraries(unittest ${TEST_LIBRARIES})
target_include_directories(unittest PRIVATE ${TEST_INCLUDE_DIRS})
message(STATUS "TEST_LIBRARIES = ${TEST_LIBRARIES}")
message(STATUS "TEST_INCLUDE_DIRS = ${TEST_INCLUDE_DIRS}")
gtest_discover_tests(unittest DISCOVERY_MODE PRE_TEST)
# gtest_discover_tests(unittest PROPERTIES
#                      ENVIRONMENT
#                      "${RAPIDJSON_PYTHON_ENV}")


add_definitions(-DYGG_TEST)
# This is required for running tests with valgrind
# if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
#     add_custom_command(TARGET unittest POST_BUILD
#             COMMAND dsymutil unittest
#             WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
#             COMMENT "Creating debug symbols...")
# endif(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
   set(CMAKE_CXX_FLAGS_DEBUG "/Zi -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELEASE "/Zi -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/Zi -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
else ()
   set(CMAKE_CXX_FLAGS_DEBUG "-g ${CMAKE_CXX_FLAGS_DEBUG}")
   set(CMAKE_CXX_FLAGS_RELEASE "-g ${CMAKE_CXX_FLAGS_DEBUG}")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g ${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
endif()

#set_tests_properties(unittest PROPERTIES
#        ENVIRONMENT
#        "${RAPIDJSON_PYTHON_ENV}")


option(YGG_SKIP_VALGRIND_TESTS "Disable valgrind tests" OFF)
if(NOT MSVC)
    find_program(VALGRIND_FOUND valgrind)
    if((NOT YGG_SKIP_VALGRIND_TESTS) AND VALGRIND_FOUND)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DRAPIDJSON_DONT_IMPORT_NUMPY")
        # Not running SIMD.* unit test cases for Valgrind
        add_test(NAME valgrind_unittest
                 COMMAND valgrind --suppressions=${CMAKE_SOURCE_DIR}/test/valgrind.supp --suppressions=${CMAKE_SOURCE_DIR}/test/valgrind-python.supp --leak-check=full --error-exitcode=1 --track-origins=yes --dsymutil=no --keep-debuginfo=yes --read-var-info=yes unittest --gtest_filter=-SIMD.*)
                 # WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
        set_tests_properties(valgrind_unittest PROPERTIES
                ENVIRONMENT
                "${RAPIDJSON_PYTHON_ENV}")
    endif((NOT YGG_SKIP_VALGRIND_TESTS) AND VALGRIND_FOUND)

endif(NOT MSVC)
