cmake_minimum_required (VERSION 3.5)
if(NOT BUILD_CPP_LIBRARY)
  return()
endif()
include(DynamicTestLibraries)
add_dynamic_test_library(
  example_c
  SOURCES example_c.c
  LIBRARIES ${YGG_TARGET_CPP}
)
