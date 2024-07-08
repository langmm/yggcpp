include(YggAddFortranSubdirectory)
include(CheckDLL)
add_mixed_fortran_library(
  example_fortran SHARED LANGUAGE CXX example_fortran.F90
  LIBRARIES ${YGG_TARGET_FORTRAN}
  INCLUDES ${YGG_FORTRAN_MOD_DIR}
)
# add_library(example_fortran SHARED example_fortran.F90)
# set_target_properties(example_fortran PROPERTIES LINKER_LANGUAGE CXX)
# target_link_libraries(example_fortran PUBLIC ${YGG_TARGET_FORTRAN})
# target_include_directories(example_fortran PUBLIC ${YGG_FORTRAN_MOD_DIR})
if (WIN32)
  set_target_properties(example_fortran PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
endif()
show_symbols(example_fortran AFTER_TARGET example_fortran)
