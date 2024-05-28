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

function(check_language_external language)
  set(options REQUIRED)
  set(oneValueArgs OUTPUT_VARIABLE GENERATOR EXTERNAL_PATHS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(tmp_dir "${CMAKE_CURRENT_BINARY_DIR}/_check_for_${language}")
  set(fcompiler "${tmp_dir}/${language}_compiler")
  file(MAKE_DIRECTORY "${tmp_dir}")
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/check_language_external.CMakeLists.in
    ${tmp_dir}/CMakeLists.txt
    @ONLY)
  execute_process(
    COMMAND ${CMAKE_COMMAND} -B . -S .
    WORKING_DIRECTORY ${tmp_dir}
    RESULT_VARIABLE out)
  if (EXISTS ${fcompiler})
    file(READ ${fcompiler} CONTENTS)
    set(CMAKE_${language}_COMPILER ${CONTENTS})
  elseif(MSVC AND language STREQUAL "Fortran")
    find_mingw_gfortran()
    if (MINGW_GFORTRAN)
      set(CMAKE_${language}_COMPILER ${MINGW_GFORTRAN})
      if (ARGS_OUTPUT_VARIABLE)
        set(${ARGS_OUTPUT_VARIABLE} ON PARENT_SCOPE)
      endif()
      if (ARGS_GENERATOR)
        set(${ARGS_GENERATOR} "MinGW Makefiles" PARENT_SCOPE)
      endif()
      if (ARGS_EXTERNAL_PATHS)
        set(${ARGS_EXTERNAL_PATHS} ${MINGW_PATH} PARENT_SCOPE)
      endif()
    endif()
  endif()
  if (NOT CMAKE_${language}_COMPILER AND ARGS_REQUIRED)
    message(FATAL_ERROR "Could not locate a ${language} compiler")
  endif()
  set(CMAKE_${language}_COMPILER ${CMAKE_${language}_COMPILER} PARENT_SCOPE)
endfunction()

function(find_mingw_gfortran)
  set(options REQUIRED)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  find_program(MINGW_GFORTRAN
    NAMES gfortran
    PATHS
      c:/MinGW/bin
      "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\MinGW;InstallLocation]/bin"
    )
  if(NOT MINGW_GFORTRAN)
    if (ARGS_REQUIRED)
      message(FATAL_ERROR
        "gfortran not found, please install MinGW with the gfortran option."
        "Or set the cache variable MINGW_GFORTRAN to the full path. "
        " This is required to build")
    endif()
    return()
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
    if (ARGS_REQUIRED)
      message(FATAL_ERROR
        "MINGW_GFORTRAN is set to\n"
        "  ${MINGW_GFORTRAN}\n"
        "which is not a MinGW gfortran for this architecture.  "
        "The output from -v does not match \"${_mingw_target}\":\n"
        "${out}\n"
        "Set MINGW_GFORTRAN to a proper MinGW gfortran for this architecture."
      )
    endif()
  endif()
  set(MINGW_GFORTRAN ${MINGW_GFORTRAN} PARENT_SCOPE)
endfunction()

function(_setup_mingw_config_and_build source_dir build_dir tmp_dir)
  # Look for a MinGW gfortran.
  find_mingw_gfortran(REQUIRED)

  # Configure scripts to run MinGW tools with the proper PATH.
  get_filename_component(MINGW_PATH ${MINGW_GFORTRAN} PATH)
  file(TO_NATIVE_PATH "${MINGW_PATH}" MINGW_PATH)
  string(REPLACE "\\" "\\\\" MINGW_PATH "${MINGW_PATH}")
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/config_mingw.cmake.in
    ${build_dir}/external_config.cmake
    @ONLY)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/build_mingw.cmake.in
    ${build_dir}/external_build.cmake
    @ONLY)
  set(source_dir ${tmp_dir})
  if (ARGS_CMAKE_COMMAND_LINE)
    set(ARGS_CMAKE_COMMAND_LINE "${ARGS_CMAKE_COMMAND_LINE} -B ${tmp_dir}")
  else()
    set(ARGS_CMAKE_COMMAND_LINE "-B ${tmp_dir}")
  endif()
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/config_mingw.cmake.in
    ${tmp_dir}/external_find_fortran.cmake
    @ONLY)
endfunction()

function(_setup_native_config_and_build source_dir build_dir tmp_dir)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/config_native.cmake.in
    ${build_dir}/external_config.cmake
    @ONLY)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/build_native.cmake.in
    ${build_dir}/external_build.cmake
    @ONLY)
  set(source_dir ${tmp_dir})
  if (ARGS_CMAKE_COMMAND_LINE)
    set(ARGS_CMAKE_COMMAND_LINE "${ARGS_CMAKE_COMMAND_LINE} -B ${tmp_dir}")
  else()
    set(ARGS_CMAKE_COMMAND_LINE "-B ${tmp_dir}")
  endif()
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/config_native.cmake.in
    ${tmp_dir}/external_find_fortran.cmake
    @ONLY)
endfunction()

function(target_link_external_fortran_objects target fortran_target)
    include(CreateMSVCLib)
    if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
        # TODO: Use CMAKE_Fortran_PREPROCESS_SOURCE
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
    add_custom_command(
        TARGET ${target}
	PRE_LINK
	COMMAND ${CMAKE_COMMAND} "-DOBJS=$<JOIN:$<TARGET_OBJECTS:${fortran_target}>,\;>" -P ${CMAKE_CURRENT_BINARY_DIR}/copy_mod.cmake
	COMMAND_EXPAND_LISTS)
    set_source_files_properties(
      ${${fortran_target}_EXT_OBJ}
      PROPERTIES
      EXTERNAL_OBJECT true
      GENERATED true)
    get_target_property(link_lib ${fortran_target} INTERFACE_LINK_LIBRARIES)
    get_target_property(link_dir ${fortran_target} INTERFACE_LINK_DIRECTORIES)
    message(STATUS "INTERFACE_LINK_LIBRARIES = ${link_lib}")
    message(STATUS "INTERFACE_LINK_DIRECTORIES = ${link_dir}")
    target_link_libraries(${target} PRIVATE $<TARGET_PROPERTY:${fortran_target},INTERFACE_LINK_LIBRARIES>)
    target_link_directories(${target} PRIVATE $<TARGET_PROPERTY:${fortran_target},INTERFACE_LINK_DIRECTORIES>)
    target_sources(${target} PRIVATE "$<TARGET_OBJECTS:${fortran_target}>")
    if (WIN32)
      create_lib_for_target(${target} SOURCE_TARGET ${fortran_target})
    endif()
endfunction()

function(add_mixed_fortran_library target library_type)
  set(oneValueArgs LANGUAGE)
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
    string(REGEX MATCH "[.][fF]((90)|(95)|(03)|(08)|(18))?$" match ${src})
    if(match)
      list(APPEND fortran_sources ${src})
    else()
      list(APPEND other_sources ${src})
    endif()
  endforeach()
  set(fortran_target ${target}_FORTRAN_OBJECT_LIBRARY)
  add_external_fortran_library(
      ${fortran_target} OBJECT
      SOURCES ${fortran_sources})
  # if(MSVC AND library_type STREQUAL "SHARED")
  #   set(library_type STATIC)
  # endif()
  if(ARGS_LANGUAGE AND MSVC)
    set_source_files_properties(
      ${other_sources} PROPERTIES LANGUAGE ${ARGS_LANGUAGE})
  endif()
  add_library(${target} ${library_type} ${other_sources})
  if(ARGS_LANGUAGE)
    set_target_properties(
      ${target} PROPERTIES LINKER_LANGUAGE ${ARGS_LANGUAGE})
  endif()
  target_link_external_fortran_objects(${target} ${fortran_target})
endfunction()

function(add_external_fortran_library target_name library_type)
  # Parse arguments to function
  set(oneValueArgs SOURCE_DIRECTORY LIBRARY_TYPE)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES CMAKE_COMMAND_LINE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(orig_source_dir "${CMAKE_CURRENT_SOURCE_DIR}/${ARGS_SOURCE_DIRECTORY}")
  set(build_dir "${CMAKE_CURRENT_BINARY_DIR}/${target_name}")
  set(source_dir "${build_dir}/src")
  set(tmp_dir "${build_dir}/find_fortran")

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
  set(${target_name}_EXT_SRC "${${target_name}_EXT_SRC}" PARENT_SCOPE)
  set(${target_name}_EXT_OBJ "${${target_name}_EXT_OBJ}" PARENT_SCOPE)
  message(STATUS "${target_name}_EXT_SRC = ${${target_name}_EXT_SRC}")
  message(STATUS "${target_name}_EXT_OBJ = ${${target_name}_EXT_OBJ}")
  set(EXTERNAL_PRODUCTS "${${target_name}_EXT_OBJ}") # ${${target_name}_EXT_DEF} ${${target_name}_EXT_EXP}")
  # if we have MSVC without Intel fortran then setup
  # external projects to build with mingw fortran
  # if(NOT (MSVC AND (NOT CMAKE_Fortran_COMPILER)))
  set(MSVC_AND_GNU_BUILD)
  if (MSVC)
    # TODO: Only do this if MSVC w/ gfortran
    set(MSVC_AND_GNU_BUILD ON)
  endif()
  set(MSVC_AND_GNU_BUILD ${MSVC_AND_GNU_BUILD} PARENT_SCOPE)
  if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC_AND_GNU_BUILD))
    enable_language(Fortran)
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
    # TODO: Use CMAKE_Fortran_PREPROCESS_SOURCE
    set(final_library_flags "${final_library_flags} -cpp")
  endif()
  if (NOT CMAKE_${final_library_type}_LIBRARY_PREFIX_Fortran)
    set(CMAKE_${final_library_type}_LIBRARY_PREFIX_Fortran ${CMAKE_${final_library_type}_LIBRARY_PREFIX})
  endif()
  message(STATUS "CMAKE_${final_library_type}_LIBRARY_SUFFIX = ${CMAKE_${final_library_type}_LIBRARY_SUFFIX}")
  message(STATUS "CMAKE_${final_library_type}_LIBRARY_SUFFIX_C = ${CMAKE_${final_library_type}_LIBRARY_SUFFIX_C}")
  message(STATUS "CMAKE_${final_library_type}_LIBRARY_SUFFIX_CXX = ${CMAKE_${final_library_type}_LIBRARY_SUFFIX_CXX}")
  message(STATUS "CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran = ${CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran}")
  if (NOT CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran)
    set(CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran ${CMAKE_${final_library_type}_LIBRARY_SUFFIX})
  endif()
  cmake_path(APPEND FINAL_LIBRARY "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_${final_library_type}_LIBRARY_PREFIX_Fortran}${target_name}${CMAKE_${final_library_type}_LIBRARY_SUFFIX_Fortran}")
  message(STATUS "FINAL_LIBRARY = ${FINAL_LIBRARY}")

  # Determine import library file name
  if(MSVC AND ${library_type} STREQUAL "SHARED")
    # message(STATUS "CMAKE_IMPORT_LIBRARY_SUFFIX = ${CMAKE_IMPORT_LIBRARY_SUFFIX}")
    # message(STATUS "CMAKE_IMPORT_LIBRARY_SUFFIX_C = ${CMAKE_IMPORT_LIBRARY_SUFFIX_C}")
    # message(STATUS "CMAKE_IMPORT_LIBRARY_SUFFIX_CXX = ${CMAKE_IMPORT_LIBRARY_SUFFIX_CXX}")
    # message(STATUS "CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran = ${CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran}")
    if (NOT CMAKE_IMPORT_LIBRARY_PREFIX_Fortran)
      set(CMAKE_IMPORT_LIBRARY_PREFIX_Fortran ${CMAKE_IMPORT_LIBRARY_PREFIX})
    endif()
    if (NOT CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran)
      set(CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran ${CMAKE_IMPORT_LIBRARY_SUFFIX})
    endif()
    cmake_path(APPEND FINAL_LIBRARY_IMPLIB "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_IMPORT_LIBRARY_PREFIX_Fortran}${target_name}${CMAKE_IMPORT_LIBRARY_SUFFIX_Fortran}")
    # cmake_path(APPEND FINAL_LIBRARY_IMPLIB "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_IMPORT_LIBRARY_PREFIX_Fortran}${target_name}.lib")
    message(STATUS "FINAL_LIBRARY_IMPLIB = ${FINAL_LIBRARY_IMPLIB}")
  endif()

  include(AddTargetsFromFile)
  generate_target_file("${target_name}.targets" TARGETS ${targets}
                       DIRECTORY ${source_dir}
		       OUTPUT_VAR target_file
		       CUSTOM_TARGET "generate_target_file_${target_name}")

  # create the external project cmake file
  file(MAKE_DIRECTORY "${source_dir}")
  file(MAKE_DIRECTORY "${tmp_dir}")
  set(external_sources ${${target_name}_EXT_SRC})
  cmake_path(APPEND external_target_file "${source_dir}" "${target_name}.external_targets")
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/external.CMakeLists.in
    ${source_dir}/CMakeLists.txt
    @ONLY)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/find_gfortran.CMakeLists.in
    ${tmp_dir}/CMakeLists.txt
    @ONLY)
  list(APPEND EXTERNAL_PRODUCTS ${external_target_file})

  # create build and configure wrapper scripts
  # check_language(Fortran)
  if(MSVC_AND_GNU_BUILD)
    _setup_mingw_config_and_build("${source_dir}" "${build_dir}" "${tmp_dir}")
  else()
    _setup_native_config_and_build("${source_dir}" "${build_dir}" "${tmp_dir}")
  endif()
  set(CONFIGURE_COMMAND
    ${CMAKE_COMMAND} -P ${build_dir}/external_config.cmake)
  set(BUILD_COMMAND
    ${CMAKE_COMMAND} -P ${build_dir}/external_build.cmake)
  # if(MSVC_AND_GNU_BUILD)
  include(${tmp_dir}/external_find_fortran.cmake)
  # endif()

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
  message(STATUS "target_name = ${target_name}")
  message(STATUS "FINAL_LIBRARY = ${FINAL_LIBRARY}")
  message(STATUS "${target_name}_EXT_OBJ = ${${target_name}_EXT_OBJ}")
  message(STATUS "CMAKE_CURRENT_BINARY_DIR = ${CMAKE_CURRENT_BINARY_DIR}")
  set_target_properties(${target_name} PROPERTIES
                        IMPORTED_LOCATION ${FINAL_LIBRARY}
			# IMPORTED_OBJECTS ${${target_name}_EXT_OBJ}
			INTERFACE_LINK_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR})
  set_property(TARGET ${target_name}
               PROPERTY IMPORTED_OBJECTS ${${target_name}_EXT_OBJ})
  if(ORIG_LIBRARIES)
    set_target_properties(
      ${target_name} PROPERTIES
      INTERFACE_LINK_LIBRARIES ${ORIG_LIBRARIES})
  endif()
  # if(MSVC_AND_GNU_BUILD)
  target_link_from_file(${target_name} IMPORTED ${external_target_file})
  # endif()
  if(WIN32 AND ${library_type} STREQUAL "SHARED")
    set_target_properties(
      ${target_name} PROPERTIES
      IMPORTED_IMPLIB ${FINAL_LIBRARY_IMPLIB})
  endif()

endfunction()
