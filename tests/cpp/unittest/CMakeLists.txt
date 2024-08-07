include(CheckCXXCompilerFlag)
include(DynamicTestLibraries)

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

include(GoogleTest)
file(GLOB files "${CMAKE_CURRENT_SOURCE_DIR}/tests/communicators/*.cpp" "${CMAKE_CURRENT_SOURCE_DIR}/tests/utils/*.cpp" "${CMAKE_CURRENT_SOURCE_DIR}/tests/datatypes/dtypetest.cpp")
generate_dynamic_tests(
  "${CMAKE_CURRENT_SOURCE_DIR}/tests/communicators/functioncommtest_language.cpp.in"
  APPEND_TO files
)

include_directories(${TEST_INCLUDE_DIRS})
if(CONDA_PREFIX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I${CONDA_INCLUDE}")
    include_directories(SYSTEM ${CONDA_INCLUDE})
endif()
add_executable(unittest ${CMAKE_CURRENT_SOURCE_DIR}/unittest.cpp ${CMAKE_CURRENT_SOURCE_DIR}/elf_hook.cpp ${CMAKE_CURRENT_SOURCE_DIR}/mock.cpp ${files})
target_link_libraries(unittest ${TEST_LIBRARIES})
target_include_directories(unittest PRIVATE ${TEST_INCLUDE_DIRS})
cmake_path(GET CMAKE_CURRENT_SOURCE_DIR PARENT_PATH PARENT_SOURCE_DIR)
cmake_path(GET CMAKE_CURRENT_BINARY_DIR PARENT_PATH PARENT_BINARY_DIR)
add_dynamic_dependencies(unittest WORKING_DIR ${PARENT_BINARY_DIR})
if (WIN32)
  set(unittest_timeout 100)
else()
  set(unittest_timeout 20)
endif()
list(APPEND unittest_args PROPERTIES TIMEOUT ${unittest_timeout})
foreach(var RAPIDJSON_PYTHON_ENV RAPIDJSON_PYTHON_ENV2 RAPIDJSON_PYTHON_ENV3)
  if (${var})
    list(APPEND unittest_args ENVIRONMENT "${${var}}")
  endif()
endforeach()
message(STATUS "unittest_args = ${unittest_args}")
# There is a bug which prevents environment variables from
# being available during test discovery on windows
# https://gitlab.kitware.com/cmake/cmake/-/issues/21453
gtest_discover_tests(unittest DISCOVERY_MODE PRE_TEST
                     WORKING_DIRECTORY ${PARENT_BINARY_DIR}
                     TEST_LIST ygg_test_list
		     ${unittest_args})


# This is required for running tests with valgrind
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
    add_custom_command(TARGET unittest POST_BUILD
            COMMAND dsymutil unittest
            WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            COMMENT "Creating debug symbols...")
endif(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
   set(CMAKE_CXX_FLAGS_DEBUG "/Zi -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELEASE "/Zi -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/Zi -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
else ()
   set(CMAKE_CXX_FLAGS_DEBUG "-g ${CMAKE_CXX_FLAGS_DEBUG}")
   set(CMAKE_CXX_FLAGS_RELEASE "-g ${CMAKE_CXX_FLAGS_DEBUG}")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g ${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
endif()

# message(STATUS "RAPIDJSON_PYTHON_ENV = ${RAPIDJSON_PYTHON_ENV}")
# message(STATUS "RAPIDJSON_PYTHON_ENV2 = ${RAPIDJSON_PYTHON_ENV2}")

include(CheckDLL)
show_runtimes(unittest)
copy_required_runtimes(unittest DEPENDENCIES ${YGG_TARGET_CXX})

if(NOT MSVC)
    find_program(VALGRIND_FOUND valgrind)
    if((NOT YGG_SKIP_VALGRIND_TESTS) AND VALGRIND_FOUND)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DRAPIDJSON_DONT_IMPORT_NUMPY")
        # Not running SIMD.* unit test cases for Valgrind
        add_test(NAME valgrind_unittest
                 COMMAND valgrind --suppressions=${PARENT_SOURCE_DIR}/valgrind.supp --suppressions=${PARENT_SOURCE_DIR}/valgrind-python.supp --leak-check=full --error-exitcode=1 --track-origins=yes --dsymutil=no --keep-debuginfo=yes --read-var-info=yes ${CMAKE_CURRENT_BINARY_DIR}/unittest --gtest_filter=-SIMD.*
                 WORKING_DIRECTORY ${PARENT_BINARY_DIR})
        set_tests_properties(valgrind_unittest PROPERTIES
                ENVIRONMENT
		"YGG_MODEL_INDEX=123")
                # "${RAPIDJSON_PYTHON_ENV}")
    endif((NOT YGG_SKIP_VALGRIND_TESTS) AND VALGRIND_FOUND)

endif(NOT MSVC)

add_custom_command(
        TARGET unittest POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
                ${CMAKE_CURRENT_SOURCE_DIR}/example_app.py
                ${PARENT_BINARY_DIR}/example_app.py)

cover_test(unittest)
