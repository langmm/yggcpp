cmake_minimum_required(VERSION 3.16)
if (NOT BUILD_Fortran_LIBRARY)
  return()
endif()
include(BuildTools)
check_language_external(Fortran)
if (NOT CMAKE_Fortran_COMPILER)
  if (YGG_Fortran_REQUIRED)
    message(FATAL_ERROR "A Fortran compiler could not be found")
  else()
    message(STATUS "A Fortran compiler could not be found")
  endif()
  return()
endif()
enable_language (C)
if ((NOT FORCE_SPLIT_CXXFortran) AND (NOT MSVC))
  enable_language(Fortran)
endif()
include(DynamicTestLibraries)
add_dynamic_test_library(
  example_fortran
  LANGUAGE Fortran
  SOURCES example_fortran.F90
  LIBRARIES ${YGG_TARGET_Fortran}
  INCLUDES ${YGG_Fortran_MOD_DIR}
)
