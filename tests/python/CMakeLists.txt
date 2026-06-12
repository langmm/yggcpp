message(STATUS "Python TESTING ON")
include(CTest)
include(SearchTools)
find_package_generic(Python COMPONENTS Interpreter)
enable_testing()
list(APPEND PYTEST_ARGS -sv)
if (NOT YGGTEST_DYNAMIC_DIR)
  set(YGGTEST_DYNAMIC_DIR ${CMAKE_CURRENT_BINARY_DIR})
endif()
list(APPEND PYTEST_ARGS --dynamic-testlib-dir=${YGGTEST_DYNAMIC_DIR})
foreach(LANGUAGE LIBRARY IN ZIP_LISTS DYNAMIC_TEST_LANGUAGES DYNAMIC_TEST_LIBRARIES)
  list(APPEND PYTEST_ARGS --external-testlib=${LANGUAGE}::${LIBRARY})
endforeach()
add_test(NAME python_tests COMMAND
         ${${Python_PREFIX}_EXECUTABLE} -m pytest ${PYTEST_ARGS}
         ${CMAKE_CURRENT_SOURCE_DIR}
	 COMMAND_EXPAND_LISTS)
if((YGG_BUILD_ASAN OR YGG_BUILD_UBSAN)
   AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(DYLD_INSERT_LIBRARIES $ENV{DYLD_INSERT_LIBRARIES})
  if(DEPS_ASAN_LIB)
    list(APPEND DYLD_INSERT_LIBRARIES ${DEPS_ASAN_LIB})
  endif()
  message(DEBUG "DYLD_INSERT_LIBRARIES = ${DYLD_INSERT_LIBRARIES}")
  if(DYLD_INSERT_LIBRARIES)
    set_tests_properties(
      python_tests PROPERTIES
      ENVIRONMENT "DYLD_INSERT_LIBRARIES=${DYLD_INSERT_LIBRARIES}"
    )
  else()
    message(WARNING "Need to set DYLD_INSERT_LIBRARIES before running the Python tests when the clang address sanitizer is used")
  endif()
endif()

include(DynamicTestLibraries)
add_dynamic_dependencies(python_tests WORKING_DIR ${YGGTEST_DYNAMIC_DIR})
