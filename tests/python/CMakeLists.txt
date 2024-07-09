message(STATUS "Python TESTING ON")
include(CTest)
include(SearchTools)
find_package_python()
enable_testing()
add_test(NAME python_tests COMMAND
         ${${PYTHON_PREFIX}_EXECUTABLE} -m pytest -sv
	 --dynamic-testlib-dir ${YGGTEST_DYNAMIC_DIR}
         ${CMAKE_CURRENT_SOURCE_DIR})
if (YGG_BUILD_ASAN AND DEFINED ENV{DYLD_INSERT_LIBRARIES})
  set_tests_properties(
    python_tests
    ENVIRONMENT "DYLD_INSERT_LIBRARIES=$ENV{DYLD_INSERT_LIBRARIES}"
  )
endif()

include(DynamicTestLibraries)
add_dynamic_dependencies(python_tests)
