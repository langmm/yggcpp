include(YggAddFortranSubdirectory)
check_language_external(Fortran)
if (NOT CMAKE_Fortran_COMPILER)
  if (YGG_Fortran_REQUIRED)
    message(FATAL_ERROR "A Fortran compiler could not be found")
  else()
    message(STATUS "A Fortran compiler could not be found")
  endif()
  return()
endif()

set(YGG_TARGET_FORTRAN YggInterface_fortran)
set(YGG_TARGET_FORTRAN ${YGG_TARGET_FORTRAN} PARENT_SCOPE)
if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
    enable_language(Fortran)
endif()

include(YggAddFortranSubdirectory)
set(SOURCES_FORTRAN c_wrappers.c YggInterface.F90)
add_mixed_fortran_library(${YGG_TARGET_FORTRAN} SHARED
                          LANGUAGE CXX ${SOURCES_FORTRAN})
add_library(YggInterface::Fortran ALIAS ${YGG_TARGET_FORTRAN})

# Configure Fortran wrapper
set_target_properties(${YGG_TARGET_FORTRAN} PROPERTIES
                      VERSION ${PROJECT_VERSION}
                      SOVERSION ${PROJECT_VERSION_MAJOR}
                      OUTPUT_NAME ${YGG_TARGET_FORTRAN})
target_include_directories(${YGG_TARGET_FORTRAN} PUBLIC
                           "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")
target_link_libraries(${YGG_TARGET_FORTRAN} PUBLIC YggInterface)
target_include_directories(${YGG_TARGET_FORTRAN} PRIVATE
                           ${DEPS_INCLUDE_DIRS})
list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_FORTRAN})
set(YGG_INSTALL_TARGETS ${YGG_INSTALL_TARGETS} PARENT_SCOPE)

# Install
list(APPEND YGG_INSTALL_INCLUDES
     ${CMAKE_CURRENT_BINARY_DIR}/ygginterface.mod)
set(YGG_INSTALL_INCLUDES ${YGG_INSTALL_INCLUDES} PARENT_SCOPE)
set(YGG_FORTRAN_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)

include(CheckDLL)
show_runtimes(${YGG_TARGET_FORTRAN})
