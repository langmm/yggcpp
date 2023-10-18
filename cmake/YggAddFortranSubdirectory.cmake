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

function(_add_fortran_library_link_interface library depend_library)
  set_target_properties(${library} PROPERTIES
    IMPORTED_LINK_INTERFACE_LIBRARIES_NOCONFIG "${depend_library}")
endfunction()


function(cmake_in_fortran_subdir project)
  set(multiValueArgs SOURCES)
  set(project_name "${project}_FORTRAN_OBJECTS")
  set(library_name "${project}_FORTRAN_OBJECTS")
  set(external_name EXTERNAL_FORTRAN_OBJECT)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(external_flag "${project}_IN_EXTERNAL_FORTRAN")
  if (${external_name} MATCHES ${project_name})
    set(${external_flag} ON CACHE BOOL "Flag specifying that the project is in the 'subdir' phase")
    project(${project_name})
    add_library(${library_name} OBJECT ${ARGS_SOURCES})
    set_target_properties(
        ${library_name} PROPERTIES
	COMPILE_FLAGS -cpp
	Fortran_STANDARD 2003
	Fortran_STANDARD_REQUIRED ON
	Fortran_MODULE_DIRECTORY ..)
  else()
    unset(${external_flag} CACHE)
  endif()
endfunction()

function(cmake_add_fortran_subdirectory_objects subdir)
  # Parse arguments to function
  set(oneValueArgs PROJECT)
  set(multiValueArgs CMAKE_COMMAND_LINE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(source_dir "${CMAKE_CURRENT_SOURCE_DIR}/${subdir}")
  set(project_name "${ARGS_PROJECT}_FORTRAN_OBJECTS")
  set(external_name EXTERNAL_FORTRAN_OBJECT)
  set(fo_dir "${subdir}/fo")
  set(library_dir "${fo_dir}")
  set(binary_dir "${fo_dir}")

  # if we are not using MSVC without fortran support
  # then just use the usual add_subdirectory to build
  # the fortran library
  check_language(Fortran)
  if(NOT MSVC)
    # if(NOT (MSVC AND (NOT CMAKE_Fortran_COMPILER)))
    set(${external_name} ${project_name} CACHE STRING "External project name")
    add_subdirectory(${subdir} ${fo_dir})
    unset(${external_name} CACHE)
    return()
  endif()
  set(CMAKE_COMMAND_LINE "-D${external_name}=${project_name}")

  # if we have MSVC without Intel fortran then setup
  # external projects to build with mingw fortran

  # use the same directory that add_subdirectory would have used
  set(build_dir "${CMAKE_CURRENT_BINARY_DIR}/${subdir}")
  foreach(dir_var library_dir binary_dir)
    if(NOT IS_ABSOLUTE "${${dir_var}}")
      get_filename_component(${dir_var}
        "${CMAKE_CURRENT_BINARY_DIR}/${${dir_var}}" ABSOLUTE)
    endif()
  endforeach()
  # create build and configure wrapper scripts
  _setup_mingw_config_and_build("${source_dir}" "${build_dir}")
  # create the external project
  externalproject_add(${project_name}_build
    SOURCE_DIR ${source_dir}
    BINARY_DIR ${build_dir}
    CONFIGURE_COMMAND ${CMAKE_COMMAND}
    -P ${build_dir}/config_mingw.cmake
    BUILD_COMMAND ${CMAKE_COMMAND}
    -P ${build_dir}/build_mingw.cmake
    BUILD_ALWAYS 1
    INSTALL_COMMAND ""
    )

  # now setup link libraries for targets
  set(start FALSE)
  set(target)
  # process anything that is left in target and target_libs
  if(DEFINED target)
    _add_fortran_library_link_interface(${target} "${target_libs}")
  endif()
endfunction()
