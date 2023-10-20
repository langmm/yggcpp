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

function(target_link_external_fortran_objects target project_name)
    if (ALLOW_UNIFIED_CXXFORTRAN AND NOT MSVC)
        set_source_files_properties(
	    ${${project_name}_EXT_SRC} PROPERTIES
	    COMPILE_FLAGS "-cpp -fPIC"
	    Fortran_STANDARD 2003
	    Fortran_STANDARD_REQUIRED ON
	    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
        target_sources(${target} PRIVATE ${${project_name}_EXT_SRC})
	return()
    endif()
    configure_file(
      ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/copy_mod.cmake.in
      ${CMAKE_CURRENT_BINARY_DIR}/copy_mod.cmake
      @ONLY)
    add_custom_command(
        TARGET ${target}
	PRE_LINK
	# COMMAND ${CMAKE_COMMAND} -E echo "IMPORTED_OBJECTS = $<TARGET_PROPERTY:${project_name},IMPORTED_OBJECTS>"
	# COMMAND ${CMAKE_COMMAND} -E echo "TARGET_OBJECTS = $<TARGET_OBJECTS:${project_name}>"
	# COMMAND ${CMAKE_COMMAND} -E echo "OBJECTS = ${${project_name}_EXT_OBJ}"
	COMMAND ${CMAKE_COMMAND} "-DOBJS=$<JOIN:$<TARGET_OBJECTS:${project_name}>,\;>" -P ${CMAKE_CURRENT_BINARY_DIR}/copy_mod.cmake
	COMMAND_EXPAND_LISTS
    )
    set_source_files_properties(
      ${${project_name}_EXT_OBJ}
      PROPERTIES
      EXTERNAL_OBJECT true
      GENERATED true)
    target_link_libraries(${target} PUBLIC ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})
    target_link_directories(${target} PUBLIC ${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES})
    target_sources(${target} PRIVATE "$<TARGET_OBJECTS:${project_name}>")
endfunction()

function(add_fortran_library project_name library_type)
  # Parse arguments to function
  set(oneValueArgs SOURCE_DIRECTORY LIBRARY_TYPE)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES CMAKE_COMMAND_LINE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(orig_source_dir "${CMAKE_CURRENT_SOURCE_DIR}/${ARGS_SOURCE_DIRECTORY}")
  set(build_dir "${CMAKE_CURRENT_BINARY_DIR}/${project_name}")
  set(source_dir "${build_dir}/src")

  # Get source & object file names
  set(${project_name}_EXT_SRC)
  foreach(src IN LISTS ARGS_SOURCES)
    if(NOT IS_ABSOLUTE "${src}")
      cmake_path(APPEND orig_source_dir ${src} OUTPUT_VARIABLE src)
      cmake_path(ABSOLUTE_PATH src NORMALIZE)
    endif()
    list(APPEND ${project_name}_EXT_SRC ${src})
  endforeach()
  set(${project_name}_EXT_OBJ)
  foreach(src IN LISTS ${project_name}_EXT_SRC)
    cmake_path(GET src FILENAME src_base)
    string(REGEX REPLACE "[.]f90$" "${CMAKE_C_OUTPUT_EXTENSION}" obj_base ${src_base})
    cmake_path(APPEND obj "${build_dir}" "${obj_base}")
    list(APPEND ${project_name}_EXT_OBJ ${obj})
  endforeach()
  set(${project_name}_EXT_SRC "${${project_name}_EXT_SRC}" PARENT_SCOPE)
  set(${project_name}_EXT_OBJ "${${project_name}_EXT_OBJ}" PARENT_SCOPE)
  message(STATUS "${project_name}_EXT_SRC = ${${project_name}_EXT_SRC}")
  message(STATUS "${project_name}_EXT_OBJ = ${${project_name}_EXT_OBJ}")
  if (ALLOW_UNIFIED_CXXFORTRAN AND NOT MSVC)
    include(FortranCInterface)
    FortranCInterface_VERIFY()
    FortranCInterface_VERIFY(CXX)
    if(NOT library_type STREQUAL "OBJECT")
      add_library(${project_name} ${library_type} ${${project_name}_EXT_SRC})
      if(ARGS_LIBRARIES)
        target_link_libraries(${project_name} PUBLIC ${ARGS_LIBRARIES})
      endif()
      if(ARGS_INCLUDES)
        target_include_directories(${project_name} PUBLIC ${ARGS_INCLUDES})
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
  cmake_path(APPEND FINAL_LIBRARY "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_${final_library_type}_LIBRARY_PREFIX_Fortran}${project_name}${CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran}")
  message(STATUS "FINAL_LIBRARY = ${FINAL_LIBRARY}")

  # Determine import library file name
  if(MSVC AND ${library_type} STREQUAL "SHARED")
    if (NOT CMAKE_IMPORT_LIBRARY_PREFIX_Fortran)
      set(CMAKE_IMPORT_LIBRARY_PREFIX_Fortran ${CMAKE_IMPORT_LIBRARY_PREFIX})
    endif()
    if (NOT CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran)
      set(CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran ${CMAKE_IMPORT_LIBRARY_SUFFIX})
    endif()
    cmake_path(APPEND FINAL_LIBRARY_IMPLIB "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_IMPORT_LIBRARY_PREFIX_Fortran}${project_name}${CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran}")
    message(STATUS "FINAL_LIBRARY_IMPLIB = ${FINAL_LIBRARY_IMPLIB}")
  endif()
  
  set(target_file)
  message(STATUS "targets = ${targets}")
  if(targets)
    cmake_path(APPEND target_file ${source_dir}
               "${project_name}_targets.txt")
    file(GENERATE OUTPUT ${target_file}
         CONTENT "$<TARGET_FILE_DIR:${targets}>")
  endif()
  message(STATUS "target_file = ${target_file}")

  # create the external project cmake file
  file(MAKE_DIRECTORY "${source_dir}")
  set(external_sources ${${project_name}_EXT_SRC})
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
  set(external_project_name ${project_name}_build)
  externalproject_add(${external_project_name}
    SOURCE_DIR ${source_dir}
    BINARY_DIR ${build_dir}
    CONFIGURE_COMMAND ${CONFIGURE_COMMAND}
    BUILD_COMMAND ${BUILD_COMMAND}
    BUILD_ALWAYS 1
    BUILD_BYPRODUCTS ${${project_name}_EXT_OBJ}
    INSTALL_COMMAND ""
    DEPENDS ${targets}
    )

  # create import library for other projects to link to
  SET_SOURCE_FILES_PROPERTIES(
    ${${project_name}_EXT_OBJ}
    PROPERTIES
    EXTERNAL_OBJECT true
    GENERATED true)
  add_library(${project_name} ${library_type} IMPORTED)
  add_dependencies(${project_name} ${external_project_name})
  set_target_properties(${project_name} PROPERTIES
                        IMPORTED_LOCATION ${FINAL_LIBRARY}
			IMPORTED_OBJECTS ${${project_name}_EXT_OBJ}
			INTERFACE_LINK_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR})
  if(ORIG_LIBRARIES)
    set_target_properties(
      ${project_name} PROPERTIES
      IMPORTED_LINK_INTERFACE_LIBRARIES ${ORIG_LIBRARIES})
  endif()
  if(MSVC AND ${library_type} STREQUAL "SHARED")
    set_target_properties(
      ${project_name} PROPERTIES
      IMPORTED_IMPLIB ${FINAL_LIBRARY_IMPLIB})
  endif()

endfunction()
