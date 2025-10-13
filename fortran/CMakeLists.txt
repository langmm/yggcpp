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

set(YGG_TARGET_Fortran YggInterface_fortran)
set(YGG_TARGET_Fortran ${YGG_TARGET_Fortran} PARENT_SCOPE)
if ((NOT FORCE_SPLIT_CXXFortran) AND (NOT MSVC))
    enable_language(Fortran)
endif()

set(SOURCES_Fortran c_wrappers.c YggInterface.F90)
if (YGG_Fortran_WRAP_C_ENUMS)
    list(APPEND SOURCES_Fortran c_wrappers_enums.c)
endif()
set(FORCE_EXTERNAL)
if(FORCE_SPLIT_CXXFortran)
  set(FORCE_EXTERNAL FORCE_EXTERNAL)
endif()
add_mixed_language_library(
  ${YGG_TARGET_Fortran} SHARED
  LINKER_LANGUAGE CXX
  BASE_LANGUAGE C
  SOURCES ${SOURCES_Fortran}
  ${FORCE_EXTERNAL}
)

# add_mixed_fortran_library(${YGG_TARGET_Fortran} SHARED
#                           LANGUAGE CXX ${SOURCES_Fortran})
add_library(YggInterface::Fortran ALIAS ${YGG_TARGET_Fortran})
if (YGG_Fortran_WRAP_C_ENUMS)
    target_compile_options(
        ${YGG_TARGET_Fortran} PUBLIC -DYGG_Fortran_WRAP_ENUMS)
endif()

# Configure Fortran wrapper
set_target_properties(${YGG_TARGET_Fortran} PROPERTIES
                      VERSION ${PROJECT_VERSION}
                      SOVERSION ${PROJECT_VERSION_MAJOR}
                      OUTPUT_NAME ${YGG_TARGET_Fortran})
target_include_directories(${YGG_TARGET_Fortran} PUBLIC
                           "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")
target_link_libraries(${YGG_TARGET_Fortran} PUBLIC YggInterface)
target_include_directories(${YGG_TARGET_Fortran} PRIVATE
                           ${DEPS_INCLUDE_DIRS})
list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_Fortran})
set(YGG_INSTALL_TARGETS ${YGG_INSTALL_TARGETS} PARENT_SCOPE)

# Exports
if (MINGW)
    list(APPEND EXPORT_HEADER_ARGS EMPTY)
endif()
include(GenerateExportHeaderYggdrasil)
generate_export_header_yggdrasil(
    ${YGG_TARGET_Fortran}
    BASE_NAME FYGG_API
    EXPORT_MACRO_NAME FYGG_API
    EXPORT_FILE_NAME YggInterface_fortran_export.h
    ${EXPORT_HEADER_ARGS})
set(YGG_INSTALL_INCLUDES ${YGG_INSTALL_INCLUDES} PARENT_SCOPE)

# Install
list(APPEND YGG_INSTALL_INCLUDES
     ${CMAKE_CURRENT_BINARY_DIR}/ygginterface.mod)
set(YGG_INSTALL_INCLUDES ${YGG_INSTALL_INCLUDES} PARENT_SCOPE)
set(YGG_Fortran_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)

if (COVERAGE_C_COMPILE_FLAGS)
    target_compile_options(
        ${YGG_TARGET_Fortran} PUBLIC
	"$<$<COMPILE_LANGUAGE:C>:${COVERAGE_C_COMPILE_FLAGS}>"
	"$<$<COMPILE_LANGUAGE:CXX>:${COVERAGE_C_COMPILE_FLAGS}>")
endif()
if (COVERAGE_C_LINK_FLAGS)
    target_link_options(
        ${YGG_TARGET_Fortran} PUBLIC
	"$<$<LINK_LANGUAGE:C>:${COVERAGE_C_LINK_FLAGS}>"
	"$<$<LINK_LANGUAGE:CXX>:${COVERAGE_C_LINK_FLAGS}>")
endif()
if (COVERAGE_C_LIBRARIES)
    target_link_libraries(
        ${YGG_TARGET_Fortran} PRIVATE ${COVERAGE_C_LIBRARIES})
endif()

include(CheckDLL)
show_runtimes(${YGG_TARGET_Fortran})
show_symbols(${YGG_TARGET_Fortran})
