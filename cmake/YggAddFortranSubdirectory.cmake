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
    if (ALLOW_SIMPLIFIED AND NOT MSVC)
        set_source_files_properties(
	    ${${project_name}_SOURCES} PROPERTIES
	    COMPILE_FLAGS "-cpp -fPIC"
	    Fortran_STANDARD 2003
	    Fortran_STANDARD_REQUIRED ON
	    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
        target_sources(${target} PRIVATE ${${project_name}_SOURCES})
	return()
    endif()
    add_custom_command(
        TARGET ${target}
	PRE_LINK
	# COMMAND ${CMAKE_COMMAND} -E echo "IMPORTED_OBJECTS = $<TARGET_PROPERTY:${project_name},IMPORTED_OBJECTS>"
	# COMMAND ${CMAKE_COMMAND} -E echo "TARGET_OBJECTS = $<TARGET_OBJECTS:${project_name}>"
	# COMMAND ${CMAKE_COMMAND} -E echo "OBJECTS = ${${project_name}_OBJECTS}"
	COMMAND ${CMAKE_COMMAND} -E copy "$<PATH:REMOVE_FILENAME,$<TARGET_OBJECTS:${project_name}>>*.mod" ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND_EXPAND_LISTS
    )
    set_source_files_properties(
      ${${project_name}_OBJECTS}
      PROPERTIES
      EXTERNAL_OBJECT true
      GENERATED true)
    target_link_libraries(${target} PUBLIC ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})
    target_link_directories(${target} PUBLIC ${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES})
    target_sources(${target} PRIVATE "$<TARGET_OBJECTS:${project_name}>")
endfunction()

function(cmake_precompile_fortran_objects project_name)
  # Parse arguments to function
  set(oneValueArgs SOURCE_DIRECTORY)
  set(multiValueArgs SOURCES CMAKE_COMMAND_LINE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(orig_source_dir "${CMAKE_CURRENT_SOURCE_DIR}/${ARGS_SOURCE_DIRECTORY}")
  set(build_dir "${CMAKE_CURRENT_BINARY_DIR}/${project_name}")
  set(source_dir "${build_dir}/src")

  if (NOT DONT_CHECK_FORTRAN_C_COMPAT)
    include(FortranCInterface)
    FortranCInterface_VERIFY()
    FortranCInterface_VERIFY(CXX)
  endif()

  # Get source & object file names
  set(SOURCES)
  foreach(src IN LISTS ARGS_SOURCES)
    if(NOT IS_ABSOLUTE "${src}")
      get_filename_component(src "${orig_source_dir}/${src}" ABSOLUTE)
    endif()
    list(APPEND SOURCES ${src})
  endforeach()
  set(OBJECTS)
  foreach(src IN LISTS SOURCES)
    cmake_path(GET src FILENAME src_base)
    string(REGEX REPLACE "[.]f90$" ".f90${CMAKE_C_OUTPUT_EXTENSION}" obj_base ${src_base})
    cmake_path(APPEND obj "${build_dir}" "${obj_base}")
    list(APPEND OBJECTS ${obj})
  endforeach()
  message(STATUS "SOURCES = ${SOURCES}")
  message(STATUS "OBJECTS = ${OBJECTS}")
  if (ALLOW_SIMPLIFIED AND NOT MSVC)
    set(${project_name}_SOURCES ${SOURCES} PARENT_SCOPE)
    return()
  endif()

  # Determine object library file name
  if (NOT CMAKE_STATIC_LIBRARY_PREFIX_Fortran)
    set(CMAKE_STATIC_LIBRARY_PREFIX_Fortran ${CMAKE_STATIC_LIBRARY_PREFIX})
  endif()
  if (NOT CMAKE_STATIC_LIBRARY_SUFFIX_Fortran)
    set(CMAKE_STATIC_LIBRARY_SUFFIX_Fortran ${CMAKE_STATIC_LIBRARY_SUFFIX})
  endif()
  cmake_path(APPEND OBJECT_LIBRARY "${build_dir}" "${CMAKE_STATIC_LIBRARY_PREFIX_Fortran}${project_name}${CMAKE_STATIC_LIBRARY_SUFFIX_Fortran}")
  message(STATUS "OBJECT_LIBRARY = ${OBJECT_LIBRARY}")
  
  # create the external project cmake file
  file(MAKE_DIRECTORY "${source_dir}")
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
    BUILD_BYPRODUCTS ${OBJECTS}
    INSTALL_COMMAND ""
    )

  # create import library for other projects to link to
  SET_SOURCE_FILES_PROPERTIES(
    ${OBJECTS}
    PROPERTIES
    EXTERNAL_OBJECT true
    GENERATED true)
  add_library(${project_name} OBJECT IMPORTED)
  add_dependencies(${project_name} ${external_project_name})
  set_target_properties(${project_name} PROPERTIES
                        IMPORTED_LOCATION ${OBJECT_LIBRARY}
			# INCLUDE_DIRECTORIES ${build_dir}
			IMPORTED_OBJECTS ${OBJECTS})

endfunction()
