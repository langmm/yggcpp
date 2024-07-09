cmake_minimum_required (VERSION 3.5)
enable_language (C)
if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
  enable_language(Fortran)
endif()
include(DynamicTestLibraries)
add_dynamic_test_library(
  example_fortran
  LANGUAGE Fortran
  SOURCES example_fortran.F90
  LIBRARIES ${YGG_TARGET_FORTRAN}
  INCLUDES ${YGG_FORTRAN_MOD_DIR}
)
