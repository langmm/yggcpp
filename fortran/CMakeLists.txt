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

option(YGG_Fortran_STANDALONE "Build Fortran interface as its own library, including the CXX sources rather than linking to the CXX interface library" OFF)
option(YGG_Fortran_STANDALONE_LINK_EXISTING_CXX "Link Fortran library against an existing installation of the CXX YggInterface library" OFF)
if(YGG_Fortran_STANDALONE_LINK_EXISTING_CXX)
  if(NOT TARGET ${YGG_TARGET_CXX})
    find_package(YggInterface REQUIRED)
    set(YGG_TARGET_CXX YggInterface::${YGG_TARGET_CXX})
  endif()
  set(YGG_Fortran_STANDALONE OFF)
endif()
if(NOT (YGG_Fortran_STANDALONE OR TARGET ${YGG_TARGET_CXX}))
  message(FATAL_ERROR "YGG_Fortran_STANDALONE not set and CXX target not defined")
endif()

cmake_path(GET CMAKE_CURRENT_SOURCE_DIR PARENT_PATH PARENT_SOURCE_DIR)
cmake_path(
  APPEND PARENT_SOURCE_DIR cpp
  OUTPUT_VARIABLE SOURCE_DIR_CXX
)
cmake_path(
  APPEND SOURCE_DIR_CXX include
  OUTPUT_VARIABLE INCLUDE_DIR_CXX
)

if(NOT YGG_TARGET_Fortran)
  set(YGG_TARGET_Fortran YggInterface_fortran)
  set(YGG_TARGET_Fortran ${YGG_TARGET_Fortran} PARENT_SCOPE)
endif()
if ((NOT FORCE_SPLIT_CXXFortran) AND (NOT MSVC))
    enable_language(Fortran)
endif()

set(LIBRARY_ARGS)
set(SOURCES_Fortran c_wrappers.c YggInterface.F90)
if (YGG_Fortran_WRAP_C_ENUMS)
    list(APPEND SOURCES_Fortran c_wrappers_enums.c)
endif()
if(YGG_Fortran_STANDALONE)
  ygginterface_options_config_accum(DEPS)
  list(APPEND SOURCES_Fortran ${SOURCES_CXX})
endif()
if(FORCE_SPLIT_CXXFortran)
  list(APPEND LIBRARY_ARGS FORCE_EXTERNAL)
endif()
add_mixed_language_library(
  ${YGG_TARGET_Fortran} SHARED
  LINKER_LANGUAGE CXX
  BASE_LANGUAGE C
  SOURCES ${SOURCES_Fortran}
  ${LIBRARY_ARGS}
)

add_library(YggInterface::Fortran ALIAS ${YGG_TARGET_Fortran})
if (YGG_Fortran_WRAP_C_ENUMS)
    target_compile_options(
        ${YGG_TARGET_Fortran} PUBLIC -DYGG_Fortran_WRAP_ENUMS)
endif()

# Configure Fortran wrapper
set_target_properties(
  ${YGG_TARGET_Fortran} PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
  # DLL_NAME_WITH_SOVERSION ON
  OUTPUT_NAME ${YGG_TARGET_Fortran}
)
target_include_directories(
  ${YGG_TARGET_Fortran} PUBLIC
  "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)
if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC" AND
   CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  # Use definition to pass knowledge via interface as custom
  # target properties were only added in cmake 3.19
  target_compile_definitions(
    ${YGG_TARGET_Fortran} PUBLIC -DYGG_Fortran_GNU_AND_MSVC
  )
endif()
if(YGG_Fortran_STANDALONE)
  target_link_libraries(
    ${YGG_TARGET_Fortran} PUBLIC
    ${DEPS_LIBRARIES_Python}
  )
  if(WIN32)
    set_target_properties(
      ${YGG_TARGET_Fortran} PROPERTIES
      WINDOWS_EXPORT_ALL_SYMBOLS ON
    )
  endif()
  if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_link_options(
      ${YGG_TARGET_Fortran} PRIVATE
      /INCLUDE:yggdrasil_rapidjson_ARRAY_API
    )
  endif()
  ygginterface_target_config(${YGG_TARGET_Fortran} PUBLIC DEPS)
  target_include_directories(
    ${YGG_TARGET_Fortran} PUBLIC
    $<BUILD_INTERFACE:${INCLUDE_DIR_CXX}>
    $<INSTALL_INTERFACE:include>
    $<INSTALL_INTERFACE:include/YggInterface>
  )
  target_compile_definitions(
    ${YGG_TARGET_Fortran} PUBLIC ${YGG_INSTALL_DEFS}
  )
  target_compile_features(${YGG_TARGET_Fortran} PUBLIC cxx_std_14)
  include(AddYggInterface)
  add_library_dirs_to_rpath(${YGG_TARGET_Fortran})
else()
  target_link_libraries(${YGG_TARGET_Fortran} PUBLIC ${YGG_TARGET_CXX})
endif()
target_include_directories(
  ${YGG_TARGET_Fortran} PRIVATE
  ${DEPS_INCLUDE_DIRS}
)
list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_Fortran})
set(YGG_INSTALL_TARGETS ${YGG_INSTALL_TARGETS} PARENT_SCOPE)

# Exports
if (MINGW)
    list(APPEND EXPORT_HEADER_ARGS EMPTY)
endif()
include(GenerateExportHeaderYggdrasil)
if(YGG_Fortran_STANDALONE OR YGG_Fortran_STANDALONE_LINK_EXISTING_CXX)
  generate_export_header_yggdrasil(
    ${YGG_TARGET_Fortran} ${EXPORT_HEADER_ARGS}
  )
  list(APPEND YGG_INSTALL_INCLUDES "${INCLUDE_DIR_CXX}/")
endif()
generate_export_header_yggdrasil(
    ${YGG_TARGET_Fortran}
    BASE_NAME FYGG_API
    EXPORT_MACRO_NAME FYGG_API
    EXPORT_FILE_NAME "${YGG_TARGET_Fortran}_export.h"
    ${EXPORT_HEADER_ARGS})

# Install
list(APPEND YGG_INSTALL_INCLUDES
     ${CMAKE_CURRENT_BINARY_DIR}/ygginterface.mod)
set(YGG_INSTALL_INCLUDES ${YGG_INSTALL_INCLUDES} PARENT_SCOPE)
set(YGG_Fortran_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)

if (DEPS_COVERAGE_C_COMPILE_FLAGS)
    target_compile_options(
        ${YGG_TARGET_Fortran} PUBLIC
	"$<$<COMPILE_LANGUAGE:C>:${DEPS_COVERAGE_COMPILE_FLAGS}>"
	"$<$<COMPILE_LANGUAGE:CXX>:${DEPS_COVERAGE_COMPILE_FLAGS}>")
endif()
if (DEPS_COVERAGE_C_LINK_FLAGS)
    target_link_options(
        ${YGG_TARGET_Fortran} PUBLIC
	"$<$<LINK_LANGUAGE:C>:${DEPS_COVERAGE_COMPILE_FLAGS}>"
	"$<$<LINK_LANGUAGE:CXX>:${DEPS_COVERAGE_COMPILE_FLAGS}>")
endif()
if (DEPS_COVERAGE_C_LIBRARIES)
    target_link_libraries(
        ${YGG_TARGET_Fortran} PRIVATE ${DEPS_COVERAGE_LIBRARIES})
endif()

if(YGG_INSPECT_TARGETS)
  include(CheckDLL)
  show_runtimes(${YGG_TARGET_Fortran})
  # show_symbols(${YGG_TARGET_Fortran})
endif()
