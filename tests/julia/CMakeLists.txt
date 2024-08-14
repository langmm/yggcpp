message(STATUS "Julia TESTING ON")
include(CTest)
include(SearchTools)
enable_testing()
if (NOT YGGTEST_DYNAMIC_DIR)
  set(YGGTEST_DYNAMIC_DIR ${CMAKE_CURRENT_BINARY_DIR})
endif()
list(APPEND JLTEST_ARGS --dynamic-testlib-dir=${YGGTEST_DYNAMIC_DIR})
foreach(LANGUAGE LIBRARY IN ZIP_LISTS DYNAMIC_TEST_LANGUAGES DYNAMIC_TEST_LIBRARIES)
  list(APPEND JLTEST_ARGS --external-testlib=${LANGUAGE}::${LIBRARY})
endforeach()
add_test(NAME julia_tests COMMAND
         ${Julia_EXECUTABLE}
	 ${CMAKE_CURRENT_SOURCE_DIR}/runtests.jl
	 ${JLTEST_ARGS}
	 COMMAND_EXPAND_LISTS)
if (YGG_BUILD_ASAN AND DEFINED ENV{DYLD_INSERT_LIBRARIES})
  set_tests_properties(
    julia_tests
    ENVIRONMENT "DYLD_INSERT_LIBRARIES=$ENV{DYLD_INSERT_LIBRARIES}"
  )
endif()

include(DynamicTestLibraries)
add_dynamic_dependencies(julia_tests WORKING_DIR ${YGGTEST_DYNAMIC_DIR})

