# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
CMakeAddFortranSubdirectory
---------------------------

Add a fortran-only subdirectory, find a fortran compiler, and build.

The ``cmake_add_fortran_subdirectory`` function adds a subdirectory
to a project that contains a fortran-only subproject.  The module will
check the current compiler and see if it can support fortran.  If no
fortran compiler is found and the compiler is MSVC, then this module
will find the MinGW gfortran.  It will then use an external project to
build with the MinGW tools.  It will also create imported targets for
the libraries created.  This will only work if the fortran code is
built into a dll, so :variable:`BUILD_SHARED_LIBS` is turned on in
the project.  In addition the :variable:`CMAKE_GNUtoMS` option is set
to on, so that Microsoft ``.lib`` files are created.  Usage is as follows:

::

  cmake_add_fortran_subdirectory(
   <subdir>                # name of subdirectory
   PROJECT <project_name>  # project name in subdir top CMakeLists.txt
   CMAKE_COMMAND_LINE ...  # extra command line flags to pass to cmake
   )

#]=======================================================================]

include(CheckLanguage)
include(ExternalProject)

function(_setup_mingw_config_and_build source_dir build_dir)
  # Look for a MinGW gfortran.
  find_program(MINGW_GFORTRAN
    NAMES gfortran
    PATHS
      c:/MinGW/bin
      "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\MinGW;InstallLocation]/bin"
    )
  if(NOT MINGW_GFORTRAN)
    message(FATAL_ERROR
      "gfortran not found, please install MinGW with the gfortran option."
      "Or set the cache variable MINGW_GFORTRAN to the full path. "
      " This is required to build")
  endif()

  # Validate the MinGW gfortran we found.
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(_mingw_target "Target:.*64.*mingw")
  else()
    set(_mingw_target "Target:.*mingw32")
  endif()
  execute_process(COMMAND "${MINGW_GFORTRAN}" -v
    ERROR_VARIABLE out ERROR_STRIP_TRAILING_WHITESPACE)
  if(NOT "${out}" MATCHES "${_mingw_target}")
    string(REPLACE "\n" "\n  " out "  ${out}")
    message(FATAL_ERROR
      "MINGW_GFORTRAN is set to\n"
      "  ${MINGW_GFORTRAN}\n"
      "which is not a MinGW gfortran for this architecture.  "
      "The output from -v does not match \"${_mingw_target}\":\n"
      "${out}\n"
      "Set MINGW_GFORTRAN to a proper MinGW gfortran for this architecture."
      )
  endif()

  # Configure scripts to run MinGW tools with the proper PATH.
  get_filename_component(MINGW_PATH ${MINGW_GFORTRAN} PATH)
  file(TO_NATIVE_PATH "${MINGW_PATH}" MINGW_PATH)
  string(REPLACE "\\" "\\\\" MINGW_PATH "${MINGW_PATH}")
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/config_mingw.cmake.in
    ${build_dir}/config_mingw.cmake
    @ONLY)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/build_mingw.cmake.in
    ${build_dir}/build_mingw.cmake
    @ONLY)
endfunction()

function(_setup_native_config_and_build source_dir build_dir)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/config_native.cmake.in
    ${build_dir}/config_native.cmake
    @ONLY)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/build_native.cmake.in
    ${build_dir}/build_native.cmake
    @ONLY)
endfunction()

function(target_link_external_fortran_objects target fortran_target)
    if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
        set_source_files_properties(
	    ${${fortran_target}_EXT_SRC} PROPERTIES
	    COMPILE_FLAGS "-cpp -fPIC"
	    Fortran_STANDARD 2003
	    Fortran_STANDARD_REQUIRED ON
	    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
        target_sources(${target} PRIVATE ${${fortran_target}_EXT_SRC})
	return()
    endif()
    configure_file(
      ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/copy_mod.cmake.in
      ${CMAKE_CURRENT_BINARY_DIR}/copy_mod.cmake
      @ONLY)
    if (WIN32)
      add_custom_command(
          TARGET ${target}
	  PRE_LINK
	  COMMAND dlltool --export-all-symbols -z ${${fortran_target}_EXT_DEF} -e ${${fortran_target}_EXT_EXP} ${${fortran_target}_EXT_OBJ}
	  COMMAND_EXPAND_LISTS)
      set_source_files_properties(
          ${${fortran_target}_EXT_DEF}
	  PROPERTIES
	  HEADER_FILE_ONLY true
	  GENERATED true)
      set_source_files_properties(
          ${${fortran_target}_EXT_EXP}
	  PROPERTIES
	  EXTERNAL_OBJECT true
	  GENERATED true)
    endif()
    add_custom_command(
        TARGET ${target}
	PRE_LINK
	# COMMAND ${CMAKE_COMMAND} -E echo "IMPORTED_OBJECTS = $<TARGET_PROPERTY:${fortran_target},IMPORTED_OBJECTS>"
	# COMMAND ${CMAKE_COMMAND} -E echo "TARGET_OBJECTS = $<TARGET_OBJECTS:${fortran_target}>"
	# COMMAND ${CMAKE_COMMAND} -E echo "OBJECTS = ${${fortran_target}_EXT_OBJ}"
	COMMAND ${CMAKE_COMMAND} "-DOBJS=$<JOIN:$<TARGET_OBJECTS:${fortran_target}>,\;>" -P ${CMAKE_CURRENT_BINARY_DIR}/copy_mod.cmake
	COMMAND_EXPAND_LISTS)
    set_source_files_properties(
      ${${fortran_target}_EXT_OBJ}
      PROPERTIES
      EXTERNAL_OBJECT true
      GENERATED true)
    target_link_libraries(${target} PUBLIC ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})
    target_link_directories(${target} PUBLIC ${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES})
    target_sources(${target} PRIVATE "$<TARGET_OBJECTS:${fortran_target}>")
    if (WIN32)
      set_source_files_properties(
        ${${fortran_target}_EXT_DEF}
	PROPERTIES
	HEADER_FILE_ONLY true
	GENERATED true)
      set_source_files_properties(
        ${${fortran_target}_EXT_EXP}
	PROPERTIES
	EXTERNAL_OBJECT true
	GENERATED true)
      target_sources(${target} PRIVATE
                     ${${fortran_target}_EXT_DEF}
		     ${${fortran_target}_EXT_EXP})
    endif()
endfunction()

function(add_mixed_fortran_library target_name library_type)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_SOURCES)
    set(sources ${ARGS_SOURCES})
  else()
    set(sources ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  set(fortran_sources)
  set(other_sources)
  foreach(src IN LISTS sources)
    string(REGEX MATCH "[.]f90$" match ${src})
    if(match)
      list(APPEND fortran_sources ${src})
    else()
      list(APPEND other_sources ${src})
    endif()
  endforeach()
  set(fortran_target_name ${target_name}_FORTRAN_OBJECT_LIBRARY)
  add_external_fortran_library(
      ${fortran_target_name} OBJECT
      SOURCES ${fortran_sources})
  # if(MSVC AND library_type STREQUAL "SHARED")
  #   set(library_type STATIC)
  # endif()
  add_library(${target_name} ${library_type} ${other_sources})
  target_link_external_fortran_objects(
      ${target_name} ${target_name}_FORTRAN_OBJECT_LIBRARY)
endfunction()

function(add_external_fortran_library target_name library_type)
  # Parse arguments to function
  set(oneValueArgs SOURCE_DIRECTORY LIBRARY_TYPE)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES CMAKE_COMMAND_LINE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(orig_source_dir "${CMAKE_CURRENT_SOURCE_DIR}/${ARGS_SOURCE_DIRECTORY}")
  set(build_dir "${CMAKE_CURRENT_BINARY_DIR}/${target_name}")
  set(source_dir "${build_dir}/src")

  # Get source & object file names
  set(${target_name}_EXT_SRC)
  foreach(src IN LISTS ARGS_SOURCES)
    if(NOT IS_ABSOLUTE "${src}")
      cmake_path(APPEND orig_source_dir ${src} OUTPUT_VARIABLE src)
      cmake_path(ABSOLUTE_PATH src NORMALIZE)
    endif()
    list(APPEND ${target_name}_EXT_SRC ${src})
  endforeach()
  set(${target_name}_EXT_OBJ)
  foreach(src IN LISTS ${target_name}_EXT_SRC)
    cmake_path(GET src FILENAME src_base)
    cmake_path(APPEND obj "${build_dir}" "${src_base}${CMAKE_C_OUTPUT_EXTENSION}")
    list(APPEND ${target_name}_EXT_OBJ ${obj})
  endforeach()
  cmake_path(APPEND ${target_name}_EXT_DEF "${build_dir}" "${target_name}.def")
  cmake_path(APPEND ${target_name}_EXT_EXP "${build_dir}" "${target_name}_exports${CMAKE_C_OUTPUT_EXTENSION}")
  set(${target_name}_EXT_SRC "${${target_name}_EXT_SRC}" PARENT_SCOPE)
  set(${target_name}_EXT_OBJ "${${target_name}_EXT_OBJ}" PARENT_SCOPE)
  set(${target_name}_EXT_DEF "${${target_name}_EXT_DEF}" PARENT_SCOPE)
  set(${target_name}_EXT_EXP "${${target_name}_EXT_EXP}" PARENT_SCOPE)
  message(STATUS "${target_name}_EXT_SRC = ${${target_name}_EXT_SRC}")
  message(STATUS "${target_name}_EXT_OBJ = ${${target_name}_EXT_OBJ}")
  message(STATUS "${target_name}_EXT_DEF = ${${target_name}_EXT_DEF}")
  message(STATUS "${target_name}_EXT_EXP = ${${target_name}_EXT_EXP}")
  set(EXTERNAL_PRODUCTS "${${target_name}_EXT_OBJ} ${${target_name}_EXT_DEF} ${${target_name}_EXT_EXP}")
  if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
    include(FortranCInterface)
    FortranCInterface_VERIFY()
    FortranCInterface_VERIFY(CXX)
    if(NOT library_type STREQUAL "OBJECT")
      add_library(${target_name} ${library_type} ${${target_name}_EXT_SRC})
      if(ARGS_LIBRARIES)
        target_link_libraries(${target_name} PUBLIC ${ARGS_LIBRARIES})
      endif()
      if(ARGS_INCLUDES)
        target_include_directories(${target_name} PUBLIC ${ARGS_INCLUDES})
      endif()
    endif()
    return()
  endif()

  # Check for targets in libraries
  set(targets)
  set(new_libraries)
  set(ORIG_LIBRARIES ${ARGS_LIBRARIES})
  foreach(lib IN LISTS ARGS_LIBRARIES)
    if (TARGET ${lib})
      list(APPEND targets ${lib})
    else()
      list(APPEND new_libraries ${lib})
    endif()
  endforeach()
  # set(ARGS_LIBRARIES ${new_libraries})

  # Determine object library file name
  set(final_library_type ${library_type})
  set(final_library_flags "-fPIC")
  if(library_type STREQUAL "OBJECT")
    set(final_library_type STATIC)
    set(final_library_flags "${final_library_flags} -cpp")
  endif()
  if (NOT CMAKE_${final_library_type}_LIBRARY_PREFIX_Fortran)
    set(CMAKE_${final_library_type}_LIBRARY_PREFIX_Fortran ${CMAKE_${final_library_type}_LIBRARY_PREFIX})
  endif()
  if (NOT CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran)
    set(CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran ${CMAKE_${final_library_type}_LIBRARY_SUFFIX})
  endif()
  cmake_path(APPEND FINAL_LIBRARY "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_${final_library_type}_LIBRARY_PREFIX_Fortran}${target_name}${CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran}")
  message(STATUS "FINAL_LIBRARY = ${FINAL_LIBRARY}")

  # Determine import library file name
  if(MSVC AND ${library_type} STREQUAL "SHARED")
    if (NOT CMAKE_IMPORT_LIBRARY_PREFIX_Fortran)
      set(CMAKE_IMPORT_LIBRARY_PREFIX_Fortran ${CMAKE_IMPORT_LIBRARY_PREFIX})
    endif()
    if (NOT CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran)
      set(CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran ${CMAKE_IMPORT_LIBRARY_SUFFIX})
    endif()
    cmake_path(APPEND FINAL_LIBRARY_IMPLIB "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_IMPORT_LIBRARY_PREFIX_Fortran}${target_name}${CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran}")
    message(STATUS "FINAL_LIBRARY_IMPLIB = ${FINAL_LIBRARY_IMPLIB}")
  endif()
  
  set(target_file)
  message(STATUS "targets = ${targets}")
  if(targets)
    cmake_path(APPEND target_file ${source_dir}
               "${target_name}_targets.txt")
    file(GENERATE OUTPUT "${target_file}.$<CONFIG>"
         CONTENT "$<TARGET_FILE_DIR:${targets}>")
    add_custom_command(
        COMMAND ${CMAKE_COMMAND} "-E" "copy_if_different" "${target_file}.$<CONFIG>" "${target_file}"
	VERBATIM
	PRE_BUILD
	DEPENDS  "${target_file}.$<CONFIG>"
	OUTPUT   "${target_file}"
	COMMENT  "creating ${target_file} file ({event: PRE_BUILD}, {filename: ${target_file}})")
    message(STATUS "target_file = ${target_file}")
  endif()
  add_custom_target("generate_target_file_${target_name}" DEPENDS ${target_file})

  # create the external project cmake file
  file(MAKE_DIRECTORY "${source_dir}")
  set(external_sources ${${target_name}_EXT_SRC})
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/external.CMakeLists.in
    ${source_dir}/CMakeLists.txt
    @ONLY)

  # create build and configure wrapper scripts
  # if we have MSVC without Intel fortran then setup
  # external projects to build with mingw fortran
  check_language(Fortran)
  # if(NOT (MSVC AND (NOT CMAKE_Fortran_COMPILER)))
  if (MSVC)
    # TODO: Only do this if MSVC w/ gfortran
    _setup_mingw_config_and_build("${source_dir}" "${build_dir}")
    set(CONFIGURE_COMMAND
      ${CMAKE_COMMAND} -P ${build_dir}/config_mingw.cmake)
    set(BUILD_COMMAND
      ${CMAKE_COMMAND} -P ${build_dir}/build_mingw.cmake)
  else()
    _setup_native_config_and_build("${source_dir}" "${build_dir}")
    set(CONFIGURE_COMMAND
      ${CMAKE_COMMAND} -P ${build_dir}/config_native.cmake)
    set(BUILD_COMMAND
      ${CMAKE_COMMAND} -P ${build_dir}/build_native.cmake)
  endif()

  # create the external project
  set(external_target_name ${target_name}_build)
  externalproject_add(${external_target_name}
    SOURCE_DIR ${source_dir}
    BINARY_DIR ${build_dir}
    CONFIGURE_COMMAND ${CONFIGURE_COMMAND}
    BUILD_COMMAND ${BUILD_COMMAND}
    BUILD_ALWAYS 1
    BUILD_BYPRODUCTS ${EXTERNAL_PRODUCTS}
    INSTALL_COMMAND ""
    DEPENDS ${targets} generate_target_file_${target_name})

  # create import library for other projects to link to
  SET_SOURCE_FILES_PROPERTIES(
    ${EXTERNAL_PRODUCTS}
    PROPERTIES
    GENERATED true)
  SET_SOURCE_FILES_PROPERTIES(
    ${${target_name}_EXT_OBJ}
    PROPERTIES
    EXTERNAL_OBJECT true)
  add_library(${target_name} ${library_type} IMPORTED GLOBAL)
  add_dependencies(${target_name} ${external_target_name})
  set_target_properties(${target_name} PROPERTIES
                        IMPORTED_LOCATION ${FINAL_LIBRARY}
			IMPORTED_OBJECTS ${${target_name}_EXT_OBJ}
			INTERFACE_LINK_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR})
  if(ORIG_LIBRARIES)
    set_target_properties(
      ${target_name} PROPERTIES
      IMPORTED_LINK_INTERFACE_LIBRARIES ${ORIG_LIBRARIES})
  endif()
  if(WIN32 AND ${library_type} STREQUAL "SHARED")
    set_target_properties(
      ${target_name} PROPERTIES
      IMPORTED_IMPLIB ${FINAL_LIBRARY_IMPLIB})
  endif()

endfunction()
