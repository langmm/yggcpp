include(CheckCXXCompilerFlag)

# Unused
# set(UNITTEST_SOURCES
#         testutils.cpp
#         unittest.cpp
#         commbasetest.cpp
#         #commheadtest.cpp
#         clientcommtest.cpp
#         servercommtest.cpp
#         ipccommtest.cpp
#         zmqcommtest.cpp)

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

add_executable(unittest ${CMAKE_CURRENT_SOURCE_DIR}/unittest/unittest.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/elf_hook.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/mock.cpp ${files})
target_link_libraries(unittest ${TEST_LIBRARIES} YggInterface dl ${Python3_LIBRARIES} Python3::NumPy)
if (ZeroMQ_FOUND)
    target_link_libraries(unittest ${ZeroMQ_LIBRARY})
endif()
if (OpenMP_CXX_FOUND)
    target_link_libraries(unittest OpenMP::OpenMP_CXX)
endif()
target_include_directories(unittest PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(unittest ${Boost_LOG_LIBRARY} ${Boost_REGEX_LIBRARY})
target_include_directories(unittest PRIVATE ${RAPIDJSON_INCLUDE_DIRS})
# add_test(NAME unittest
#          COMMAND ${CMAKE_BINARY_DIR}/test/unittest
#         WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
# set_tests_properties(unittest PROPERTIES
#                      ENVIRONMENT
#                      "${RAPIDJSON_PYTHON_ENV}")
gtest_discover_tests(unittest PROPERTIES
                     ENVIRONMENT
                     "${RAPIDJSON_PYTHON_ENV}")

# foreach(ffname ${files})
#     get_filename_component(fname ${ffname} NAME_WE)
#     add_executable(unittest_${fname} ${CMAKE_CURRENT_SOURCE_DIR}/unittest/unittest.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/elf_hook.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/mock.cpp ${ffname})
#     target_link_libraries(unittest_${fname} ${TEST_LIBRARIES} YggInterface dl ${Python3_LIBRARIES} Python3::NumPy)
#     if (ZeroMQ_FOUND)
#         target_link_libraries(unittest_${fname} ${ZeroMQ_LIBRARY})
#     endif()
#     if (OpenMP_CXX_FOUND)
#         target_link_libraries(unittest_${fname} OpenMP::OpenMP_CXX)
#     endif()
#     target_include_directories(unittest_${fname} PRIVATE ${RAPIDJSON_INCLUDE_DIRS})
#     add_test(NAME unittest_${fname}
#             COMMAND ${CMAKE_BINARY_DIR}/test/unittest_${fname}
#             WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
#     set_tests_properties(unittest_${fname} PROPERTIES
#                          ENVIRONMENT
#                          "${RAPIDJSON_PYTHON_ENV}")
#     gtest_discover_tests(unittest_${fname})
# endforeach()
#foreach(ffname ${files})
#    get_filename_component(fname ${ffname} NAME_WE)
#    add_executable(unittest_${fname} ${CMAKE_CURRENT_SOURCE_DIR}/unittest/unittest.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/elf_hook.cpp ${CMAKE_CURRENT_SOURCE_DIR}/unittest/mock.cpp ${ffname})
#    target_link_libraries(unittest_${fname} ${TEST_LIBRARIES} YggInterface dl ${YGG_ZMQ_LIBS})
#    add_test(NAME unittest_${fname}
#            COMMAND ${CMAKE_BINARY_DIR}/test/unittest_${fname}
#            WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
#    if (OpenMP_CXX_FOUND)
#        target_link_libraries(unittest_${fname} OpenMP::OpenMP_CXX)
#    endif()
#    gtest_discover_tests(unittest_${fname})
#endforeach()

#add_dependencies(tests unittest)
add_definitions(-DYGG_TEST)
# if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
#     add_custom_command(TARGET unittest POST_BUILD
#             COMMAND dsymutil unittest
#             WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
#             COMMENT "Creating debug symbols...")
# endif(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
set(CMAKE_CXX_FLAGS_DEBUG "-g -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
set(CMAKE_CXX_FLAGS_RELEASE "-g -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")

#set_tests_properties(unittest PROPERTIES
#        ENVIRONMENT
#        "${YGG_PYTHON_ENV}")


option(YGG_SKIP_VALGRIND_TESTS "Disable valgrind tests" OFF)
if(NOT MSVC)
    find_program(VALGRIND_FOUND valgrind)
    if((NOT YGG_SKIP_VALGRIND_TESTS) AND VALGRIND_FOUND)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DRAPIDJSON_DONT_IMPORT_NUMPY")
        # Not running SIMD.* unit test cases for Valgrind
        add_test(NAME valgrind_unittest
                COMMAND valgrind --suppressions=${CMAKE_SOURCE_DIR}/test/valgrind.supp --suppressions=${CMAKE_SOURCE_DIR}/test/valgrind-python.supp --leak-check=full --error-exitcode=1 --track-origins=yes --dsymutil=no --keep-debuginfo=yes --read-var-info=yes ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/unittest --gtest_filter=-SIMD.*
                WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
        set_tests_properties(valgrind_unittest PROPERTIES
                ENVIRONMENT
                "${YGG_PYTHON_ENV}")
    endif((NOT YGG_SKIP_VALGRIND_TESTS) AND VALGRIND_FOUND)

    #if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    #    add_test(NAME symbol_check
    #            COMMAND sh -c "objdump -t -C libnamespacetest.a | grep rapidjson ; test $? -ne 0"
    #            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    #endif(CMAKE_BUILD_TYPE STREQUAL "Debug")

endif(NOT MSVC)
#include(GoogleTest)
#gtest_discover_tests(GeneratorTest)
