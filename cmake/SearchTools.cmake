include(GeneralTools)

macro(_initialize_find_package NO_UNPARSED)
  list(APPEND options REQUIRED)
  list(APPEND oneValueArgs FOUND_VAR VAR_PREFIX HEADER IMPORTED_TARGET)
  list(APPEND multiValueArgs LIBNAMES REQUIRED_TARGETS
       ADDITIONAL_PROPERTIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NO_UNPARSED)
    check_no_unparsed(ARGS)
  endif()
  set_options_to_names(ARGS "${options}")
  if(name MATCHES "Python")
    set_default(ARGS_VAR_PREFIX "Python")
    if(Python_PREFIX AND (NOT name STREQUAL "${Python_PREFIX}"))
      set(name "${Python_PREFIX}")
    endif()
    setup_python_search(PREFIX "${name}")
  else()
    set_default(ARGS_VAR_PREFIX "${name}")
  endif()
  set_default(ARGS_FOUND_VAR "${name}_FOUND")
  set_default(ARGS_LIBNAMES ${name})
  # set_default(ARGS_IMPORTED_TARGET ${name})
  if (NOT ARGS_HEADER)
    list(GET ARGS_LIBNAMES 0 FIRST_NAME)
    set(ARGS_HEADER ${FIRST_NAME}.h)
  endif()
endmacro()

macro(_propagate_cmake_variables_package)
  if(${ARGS_FOUND_VAR})
    propagate_cmake_variables_prefix("${ARGS_VAR_PREFIX}" ${ARGS_ADDITIONAL_PROPERTIES} ${ARGN})
  else()
    set(${ARGS_FOUND_VAR} OFF PARENT_SCOPE)
  endif()
endmacro()

macro(_finalize_find_package)
  collect_package_arguments(FINALIZE_ARGS ARGS "${options}")
  finalize_package(${name} ${FINALIZE_ARGS})
  _propagate_cmake_variables_package()
endmacro()

function(collect_package_arguments VAR PREFIX options)
  collect_arguments(
    ${VAR} ${PREFIX} "${options}"
    REQUIRED FOUND_VAR VAR_PREFIX HEADER IMPORTED_TARGET
    LIBNAMES REQUIRED_TARGETS
    ${ARGN}
    ADDITIONAL_PROPERTIES  # Must be last to ensure it dosn't absorb vars
  )
  set(${VAR} "${${VAR}}" PARENT_SCOPE)
endfunction()

function(add_library_python target)
  if (NOT Python_PREFIX)
    set(Python_PREFIX Python)
    set(Python_PREFIX Python PARENT_SCOPE)
  endif()
  cmake_parse_arguments(
    ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (Python_PREFIX STREQUAL "Python3")
    Python3_add_library(${target} ${ARGS_UNPARSED_ARGUMENTS})
  else()
    Python_add_library(${target} ${ARGS_UNPARSED_ARGUMENTS})
  endif()
endfunction()

# function(find_package_python)
#     # needed on GitHub Actions CI: actions/setup-python does not touch registry/frameworks on Windows/macOS
#     # this mirrors PythonInterp behavior which did not consult registry/frameworks first
#     if (NOT Python_PREFIX)
#       set(Python_PREFIX Python)
#       set(Python_PREFIX Python PARENT_SCOPE)
#     endif()
#     if (NOT DEFINED ${Python_PREFIX}_FIND_REGISTRY)
#         set(${Python_PREFIX}_FIND_REGISTRY "LAST")
#     endif ()
#     if (NOT DEFINED ${Python_PREFIX}_FIND_FRAMEWORK)
#         set(${Python_PREFIX}_FIND_FRAMEWORK "LAST")
#     endif ()
#     if(${Python_PREFIX}_EXECUTABLE)
#         message(DEBUG "Python executable provided ${${Python_PREFIX}_EXECUTABLE}")
#         if(NOT ${Python_PREFIX}_NumPy_INCLUDE_DIRS)
# 	    execute_process(
# 	      COMMAND ${${Python_PREFIX}_EXECUTABLE} -c "import numpy; print(numpy.get_include())"
# 	      OUTPUT_VARIABLE ${Python_PREFIX}_NumPy_INCLUDE_DIRS
# 	      RESULT_VARIABLE NUMPY_NOT_FOUND)
#             if(NUMPY_NOT_FOUND)
#                 message(FATAL_ERROR "Numpy include dirs not found")
#             endif()
#         endif()
#     else()
#         if(NOT ${Python_PREFIX}_ROOT_DIR)
#             if(CONDA_PREFIX)
#                 set(${Python_PREFIX}_ROOT_DIR "${CONDA_PREFIX}")
#             else()
#                 if(${Python_PREFIX}_EXECUTABLE)
# 		    execute_process(
# 		      COMMAND ${${Python_PREFIX}_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_config_var('base'))"
# 		      OUTPUT_VARIABLE Python_ROOT
# 		      RESULT_VARIABLE ROOT_NOT_FOUND)
#                     if(ROOT_NOT_FOUND)
#                         message(FATAL_ERROR "Python root not found")
#                     endif()
# 	            set(${Python_PREFIX}_ROOT_DIR "${Python_ROOT}")
#                 endif()
#             endif()
#         endif()
#     endif()
#     if(${Python_PREFIX}_EXECUTABLE OR ${Python_PREFIX}_ROOT_DIR)
#         # Force use of specified installation, should be enabled by
# 	# default for CMP0094=NEW and CMake >= 3.15
#         if(NOT ${Python_PREFIX}_FIND_STRATEGY)
#             set(${Python_PREFIX}_FIND_STRATEGY LOCATION)
#         endif()
#     endif()
#     if(${Python_PREFIX}_ROOT_DIR)
#         message(DEBUG "Python root directory is ${${Python_PREFIX}_ROOT_DIR}")
#         if (NOT ${Python_PREFIX}_ROOT)
#             set(${Python_PREFIX}_ROOT ${${Python_PREFIX}_ROOT_DIR})
#         endif()
#     endif()
#     find_package(${Python_PREFIX} COMPONENTS Interpreter Development NumPy REQUIRED)
#     if(NOT ${Python_PREFIX}_NumPy_FOUND)
#         message(FATAL_ERROR "NumPy headers not found")
#     endif()
#     if(NOT ${Python_PREFIX}_FOUND)
#         message(FATAL_ERROR "Python libraries not found")
#     endif()
#     if (APPLE AND ${Python_PREFIX}_EXECUTABLE)
#       execute_process(
#         COMMAND realpath ${${Python_PREFIX}_EXECUTABLE}
# 	OUTPUT_VARIABLE ${Python_PREFIX}_EXECUTABLE_FULL
# 	RESULT_VARIABLE ERROR_IN_FULL
# 	OUTPUT_STRIP_TRAILING_WHITESPACE)
#       if ((NOT ERROR_IN_FULL) AND (NOT ${Python_PREFIX}_EXECUTABLE STREQUAL "${${Python_PREFIX}_EXECUTABLE_FULL}"))
#         set(${Python_PREFIX}_EXECUTABLE ${${Python_PREFIX}_EXECUTABLE_FULL})
#       endif()
#     endif()
#     message(STATUS "${Python_PREFIX}_EXECUTABLE = ${${Python_PREFIX}_EXECUTABLE}")
#     propagate_cmake_variables_prefix("Python")
# endfunction()

function(setup_python_search)
  # needed on GitHub Actions CI: actions/setup-python does not touch registry/frameworks on Windows/macOS
  # this mirrors PythonInterp behavior which did not consult registry/frameworks first
  set(oneValueArgs PREFIX)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  check_no_unparsed(ARGS)
  if(NOT ARGS_PREFIX)
    if(Python_PREFIX)
      set(ARGS_PREFIX "${Python_PREFIX}")
    else()
      set(ARGS_PREFIX "Python")
    endif()
  endif()

  if(${ARGS_PREFIX}_SETUP_PYTHON_SEARCH_PERFORMED)
    return()
  endif()
  
  if (NOT DEFINED ${ARGS_PREFIX}_FIND_REGISTRY)
    set(${ARGS_PREFIX}_FIND_REGISTRY "LAST")
  endif ()
  if (NOT DEFINED ${ARGS_PREFIX}_FIND_FRAMEWORK)
    set(${ARGS_PREFIX}_FIND_FRAMEWORK "LAST")
  endif ()
  if(${ARGS_PREFIX}_EXECUTABLE)
    if(NOT ${ARGS_PREFIX}_NumPy_INCLUDE_DIRS)
      execute_process(
        COMMAND ${${ARGS_PREFIX}_EXECUTABLE} -c "import numpy; print(numpy.get_include())"
        OUTPUT_VARIABLE ${ARGS_PREFIX}_NumPy_INCLUDE_DIRS
        RESULT_VARIABLE NUMPY_NOT_FOUND
      )
      if(NUMPY_NOT_FOUND)
        set(${ARGS_PREFIX}_NumPy_INCLUDE_DIRS)
        message(FATAL_ERROR "Numpy include dirs not found")
      endif()
    endif()
  endif()
  if(NOT ${ARGS_PREFIX}_ROOT_DIR)
    if(CONDA_PREFIX)
      set(${ARGS_PREFIX}_ROOT_DIR "${CONDA_PREFIX}")
    elseif(${ARGS_PREFIX}_EXECUTABLE)
      execute_process(
        COMMAND ${${ARGS_PREFIX}_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_config_var('base'))"
        OUTPUT_VARIABLE ${ARGS_PREFIX}_ROOT_DIR
        RESULT_VARIABLE ROOT_NOT_FOUND
      )
      if(ROOT_NOT_FOUND)
        set(${ARGS_PREFIX}_ROOT_DIR)
      endif()
    endif()
  endif()
  if(${ARGS_PREFIX}_EXECUTABLE OR ${ARGS_PREFIX}_ROOT_DIR)
    # Force use of specified installation, should be enabled by
    # default for CMP0094=NEW and CMake >= 3.15
    if(NOT ${ARGS_PREFIX}_FIND_STRATEGY)
      set(${ARGS_PREFIX}_FIND_STRATEGY LOCATION)
    endif()
  endif()
  if(${ARGS_PREFIX}_EXECUTABLE)
    message(DEBUG "Python executable provided ${${ARGS_PREFIX}_EXECUTABLE}")
  endif()
  if(${ARGS_PREFIX}_ROOT_DIR)
    message(DEBUG "Python root directory is ${${ARGS_PREFIX}_ROOT_DIR}")
    if(NOT ${ARGS_PREFIX}_ROOT)
      set(${ARGS_PREFIX}_ROOT ${${ARGS_PREFIX}_ROOT_DIR})
    endif()
  endif()
  set(${ARGS_PREFIX}_SETUP_PYTHON_SEARCH_PERFORMED ON)
  propagate_cmake_variables_prefix("${ARGS_PREFIX}")
endfunction()

function(finalize_package name)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs)
  _initialize_find_package(ON ${ARGN})
  if(${name}_CONFIG)
    message(DEBUG "${name}_CONFIG = ${${name}_CONFIG}")
    include(${${name}_CONFIG})
  endif()
  if(${ARGS_FOUND_VAR} AND name MATCHES "Python")
    list(FIND ARGS_COMPONENTS "NumPy" IDX_NUMPY)
    if((NOT IDX_NUMPY EQUAL -1) AND (NOT ${name}_NumPy_FOUND))
      message(DEBUG "NumPy headers not found for ${name}")
      set(${ARGS_FOUND_VAR} OFF)
    elseif(APPLE AND ${name}_EXECUTABLE)
      execute_process(
        COMMAND realpath ${${name}_EXECUTABLE}
	OUTPUT_VARIABLE ${name}_EXECUTABLE_FULL
	RESULT_VARIABLE ERROR_IN_FULL
	OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      if ((NOT ERROR_IN_FULL) AND (NOT ${name}_EXECUTABLE STREQUAL "${${name}_EXECUTABLE_FULL}"))
        set(${name}_EXECUTABLE ${${name}_EXECUTABLE_FULL})
      endif()
    endif()
    set(Python_FOUND ON)
    set(Python_FOUND_PREFIX "${name}")
  endif()
  if(${ARGS_FOUND_VAR} AND (NOT ${name}_LIBRARY) AND ${name}_LIBRARIES)
    list(LENGTH ${name}_LIBRARIES NLIBS)
    if(NLIBS EQUAL 1)
      set(${name}_LIBRARY ${${name}_LIBRARIES})
    endif()
  endif()
  if(${ARGS_FOUND_VAR} AND ARGS_IMPORTED_TARGET)
    create_interface_library(
      ${name} TARGET ${ARGS_IMPORTED_TARGET}
      LIBNAMES ${ARGS_LIBNAMES}
    )
  endif()
  if(${ARGS_FOUND_VAR} AND NOT ${name}_LIBRARY_TARGET)
    foreach(itarget ${ARGS_IMPORTED_TARGET} ${name}
            ${ARGS_LIBNAMES} ${${name}_LIBRARY})
      if(TARGET ${itarget})
        set(${name}_LIBRARY_TARGET ${itarget})
        break()
      endif()
    endforeach()
  endif()
  if(${ARGS_FOUND_VAR} AND ${name}_LIBRARY AND (NOT ${name}_LIBRARY_DIR) AND
     (EXISTS ${${name}_LIBRARY}))
    cmake_path(
      REMOVE_FILENAME ${name}_LIBRARY
      OUTPUT_VARIABLE ${name}_LIBRARY_DIR
    )
  endif()
  if(${ARGS_FOUND_VAR} AND ARGS_REQUIRED_TARGETS)
    foreach(ilib IN LISTS ARGS_REQUIRED_TARGETS)
      if(NOT TARGET ${ilib})
        message(DEBUG "Required target \"${ilib}\" does not exist")
        set(${ARGS_FOUND_VAR} OFF)
        break()
      endif()
    endforeach()
  endif()
  if(ARGS_REQUIRED AND NOT ${ARGS_FOUND_VAR})
    message(FATAL_ERROR "Failed to find package \"${name}\"")
  endif()
  propagate_cmake_variables_prefix("${ARGS_VAR_PREFIX}" ${ARGS_ADDITIONAL_PROPERTIES})
endfunction()

function(create_interface_library package)
  set(oneValueArgs TARGET)
  set(multiValueArgs LIBNAMES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(target ${package})
  if(ARGS_TARGET)
    set(target ${ARGS_TARGET})
  endif()
  # From current scope variables
  if(TARGET ${target})
    message(DEBUG "${target} is already a target")
    return()
  endif()
  if(${package}_LIBRARY AND TARGET ${${package}_LIBRARY})
    message(DEBUG "Creating target \"${target}\" by aliassing \"${${package}_LIBRARY}\"")
    add_library(${target} ALIAS ${${package}_LIBRARY})
    return()
  endif()
  foreach(ilib IN LISTS ARGS_LIBNAMES)
    if(TARGET ${ilib})
      message(DEBUG "Creating target \"${target}\" by aliassing \"${ilib}\"")
      add_library(${target} ALIAS ${ilib})
      return()
    endif()
  endforeach()
  message(DEBUG "Creating target \"${target}\" from the variables in scope for package \"${package}\"")
  # dump_cmake_variables(REGEX "^${package}*" LOG_LEVEL DEBUG)
  add_library(${target} INTERFACE IMPORTED GLOBAL)
  if(${package}_LIBRARY)
    if(WIN32 AND NOT ${package}_IMPLIB)
      string(FIND ${${package}_LIBRARY} ".dll" DLL_IDX)
      if(DLL_IDX EQUAL -1)
        find_dll_from_implib(${${package}_LIBRARY} dlllib)
        if(dlllib)
          set(${package}_IMPLIB ${${package}_LIBRARY})
          set(${package}_LIBRARY ${dlllib})
        endif()
      else()
        find_implib_from_dll(${${package}_LIBRARY} implib)
        if(implib)
          set(${package}_IMPLIB ${implib})
        endif()
      endif()
    endif()
    set_property(
      TARGET ${target} PROPERTY
      IMPORTED_LOCATION ${${package}_LIBRARY}
    )
  endif()
  if(WIN32 AND ${package}_IMPLIB)
    set_property(
      TARGET ${target} PROPERTY
      IMPORTED_IMPLIB ${${package}_IMPLIB}
    )
  endif()
  if(${package}_LIBRARIES)
    add_dependencies(${target} ${${package}_LIBRARIES})
  endif()
  if(${package}_LIBRARY_DIRS)
    set_property(
      TARGET ${target} PROPERTY
      INTERFACE_LINK_DIRECTORIES ${${package}_LIBRARY_DIRS}
    )
  endif()
  if(${package}_LIBRARY OR ${package}_LINK_LIBRARIES)
    set_property(
      TARGET ${target} PROPERTY
      INTERFACE_LINK_LIBRARIES
      ${${package}_LIBRARY} ${${package}_LINK_LIBRARIES}
    )
  endif()
  if(${package}_LDFLAGS)
    set_property(
      TARGET ${target} PROPERTY
      INTERFACE_LINK_OPTIONS ${${package}_LDFLAGS}
      # ${${package}_LDFLAGS_OTHER}  # Not required
    )
  endif()
  if(${package}_INCLUDE_DIRS)
    set_property(
      TARGET ${target} PROPERTY
      INTERFACE_INCLUDE_DIRECTORIES ${${package}_INCLUDE_DIRS}
    )
  endif()
  if(${package}_CFLAGS)
    set_property(
      TARGET ${target} PROPERTY
      INTERFACE_COMPILE_OPTIONS ${${package}_CFLAGS}
      # ${${package}_CFLAGS_OTHER}  # Not required
    )
  endif()
  propagate_cmake_variables_prefix("${package}")
endfunction()

function(find_package_default name)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs)
  _initialize_find_package(OFF ${ARGN})
  find_package(${name} ${ARGS_REQUIRED} ${ARGS_UNPARSED_ARGUMENTS})
  _finalize_find_package()
endfunction()

function(find_package_generic name)
  set(options NO_DEFAULT NO_DEFAULT_CONFIG NO_PKGCONFIG NO_CONDA NO_BRUTE)
  set(oneValueArgs)
  set(multiValueArgs SEARCH_ORDER COMPONENTS
      HINTS PATHS HEADER_HINTS HEADER_PATHS)
  _initialize_find_package(ON ${ARGN})
  set_default(ARGS_SEARCH_ORDER DEFAULT PKGCONFIG CONDA)
  collect_package_arguments(BASE_ARGS ARGS "${options}")
  collect_arguments(
    SEARCH_ARGS ARGS "${options}"
    HINTS HEADER_HINTS
    PATHS HEADER_PATHS
  )
  list(APPEND SEARCH_ARGS ${BASE_ARGS})
  collect_arguments(
    DEFAULT_ARGS ARGS "${options}"
    COMPONENTS PATHS HINTS
  )
  list(APPEND DEFAULT_ARGS ${BASE_ARGS})
  message(DEBUG "find_package_generic[${name}]: HEADER = ${ARGS_HEADER}, LIBNAMES = ${ARGS_LIBNAMES}, SEARCH_ORDER = ${ARGS_SEARCH_ORDER}, DEFAULT_ARGS = ${DEFAULT_ARGS}, SEARCH_ARGS = ${SEARCH_ARGS}, REQUIRED_TARGETS = ${ARGS_REQUIRED_TARGETS}")

  foreach(method IN LISTS ARGS_SEARCH_ORDER)
    if(ARGS_NO_${method})
      continue()
    endif()
    message(DEBUG "Searching for ${name} using ${method}")
    if(method STREQUAL "DEFAULT")
      find_package_default(${name} ${DEFAULT_ARGS})
    elseif(method STREQUAL "DEFAULT_CONFIG")
      find_package_default(${name} CONFIG ${DEFAULT_ARGS})
    elseif(method STREQUAL "CONDA")
      find_package_conda(${name} ${SEARCH_ARGS})
    elseif(method STREQUAL "PKGCONFIG")
      find_package_pkgconfig(${name} ${SEARCH_ARGS})
    elseif(method STREQUAL "BRUTE")
      find_package_brute(${name} ${SEARCH_ARGS})
    else()
      message(FATAL_ERROR "Unsupported method \"${method}\"")
    endif()
    if(${ARGS_FOUND_VAR})
      message(DEBUG "${name} found using ${method}")
      break()
    else()
      message(DEBUG "${name} could not be found using ${method}")
      # dump_cmake_variables(REGEX "^${name}*" LOG_LEVEL DEBUG)
    endif()
  endforeach()

  if(${name}_CONFIG)
    message(DEBUG "${name}_CONFIG = ${${name}_CONFIG}")
    include(${${name}_CONFIG})
  endif()

  if((NOT ${ARGS_FOUND_VAR}) AND (NOT ARGS_NO_BRUTE) AND
     (NOT ${name}_BRUTE_SEARCH_PERFORMED))
    message(DEBUG "Final brute force effort to find ${name}")
    find_package_brute(${name} ${SEARCH_ARGS})
  endif()

  # _finalize_find_package()
  _propagate_cmake_variables_package()
  dump_cmake_variables(PREFIX "${ARGS_VAR_PREFIX}" LOG_LEVEL DEBUG)
endfunction()

function(find_package_brute name)
  set(optionsFind NO_PACKAGE_ROOT_PATH NO_CMAKE_PATH
      NO_CMAKE_ENVIRONMENT_PATH NO_SYSTEM_ENVIRONMENT_PATH
      NO_CMAKE_SYSTEM_PATH NO_CMAKE_INSTALL_PREFIX
      CMAKE_FIND_ROOT_PATH_BOTH ONLY_CMAKE_FIND_ROOT_PATH
      NO_CMAKE_FIND_ROOT_PATH)  # NO_CACHE
  set(oneValueArgsFind REGISTRY_VIEW VALIDATOR DOC)
  set(multiValueArgsFind PATH_SUFFIXES)
  set(options ${optionsFind})
  set(oneValueArgs ${oneValueArgsFind})
  set(multiValueArgs PATHS HINTS HEADER_PATHS HEADER_HINTS
      ${multiValueArgsFind})
  _initialize_find_package(ON ${ARGN})
  collect_arguments(
    FIND_ARGS ARGS "${options}"
    ${optionsFind} ${oneValueArgsFind} ${multiValueArgsFind}
  )

  if(NOT ${name}_INCLUDE_DIR)
    find_path(
      ${name}_INCLUDE_DIR
      NAMES ${ARGS_HEADER}
      PATHS ${ARGS_HEADER_PATHS}
      HINTS ${ARGS_HEADER_HINTS}
      ${FIND_ARGS}
    )
    if(${name}_INCLUDE_DIR STREQUAL "${name}_INCLUDE_DIR-NOTFOUND")
      message(DEBUG "Failed to find ${name}_INCLUDE_DIR")
      # foreach(dir IN LISTS ARGS_HEADER_SEARCH_PATH)
      #   execute_process(
      #     COMMAND ls ${dir}
      #     COMMAND_ECHO STDOUT
      #   )
      # endforeach()
      set(${name}_INCLUDE_DIR)
    elseif(NOT ${name}_INCLUDE_DIRS)
      set(${name}_INCLUDE_DIRS ${${name}_INCLUDE_DIR})
    endif()
  endif()

  if(NOT ${name}_LIBRARY)
    find_library(
      ${name}_LIBRARY
      NAMES ${ARGS_LIBNAMES}
      PATHS ${ARGS_PATHS}
      HINTS ${ARGS_HINTS}
      NO_CACHE
      ${FIND_ARGS}
    )
    if(${name}_LIBRARY STREQUAL "${name}_LIBRARY-NOTFOUND")
      message(DEBUG "Failed to find ${name}_LIBRARY")
      # foreach(dir IN LISTS ARGS_LIBRARY_SEARCH_PATH)
      #   execute_process(
      #     COMMAND ls ${dir}
      #     COMMAND_ECHO STDOUT
      #   )
      # endforeach()
      set(${name}_LIBRARY)
    endif()
  endif()

  if(${name}_INCLUDE_DIR AND ${name}_LIBRARY)
    set(${ARGS_FOUND_VAR} ON)
  endif()
  set(${name}_BRUTE_SEARCH_PERFORMED ON PARENT_SCOPE)

  _finalize_find_package()

endfunction()

function(find_package_conda name)
  set(oneValueArgs VAR_PREFIX)
  set(multiValueArgs ADDITIONAL_PROPERTIES PATHS HEADER_PATHS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set_default(ARGS_VAR_PREFIX "${name}")
  if(NOT CONDA_PREFIX)
    cmake_path(SET CONDA_PREFIX "$ENV{CONDA_PREFIX}")
  endif()

  if(CONDA_PREFIX)
    if (WIN32)
      set(INCLUDE_DIRS "${CONDA_PREFIX}/Library/include")
      set(LIBRARY_DIRS "${CONDA_PREFIX}/Library/lib")
    else()
      set(INCLUDE_DIRS "${CONDA_PREFIX}/include")
      set(LIBRARY_DIRS "${CONDA_PREFIX}/lib")
    endif()
    # set(ARGS_PATHS "${LIBRARY_DIRS}" ${ARGS_PATHS})
    # set(ARGS_HEADER_PATHS "${INCLUDE_DIRS}" ${ARGS_HEADER_PATHS})
    list(APPEND ARGS_PATHS "${LIBRARY_DIRS}")
    list(APPEND ARGS_HEADER_PATHS "${INCLUDE_DIRS}")
    find_package_brute(
      ${name} ${ARGS_UNPARSED_ARGUMENTS}
      VAR_PREFIX ${ARGS_VAR_PREFIX}
      PATHS ${ARGS_PATHS}
      HEADER_PATHS ${ARGS_HEADER_PATHS}
      ADDITIONAL_PROPERTIES ${ARGS_ADDITIONAL_PROPERTIES}
    )
    set(${name}_BRUTE_SEARCH_PERFORMED ON PARENT_SCOPE)
  endif()

  _propagate_cmake_variables_package()
endfunction()

function(find_package_pkgconfig name)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs PATHS HEADER_PATHS)
  _initialize_find_package(OFF ${ARGN})

  find_package(PkgConfig)
  if(NOT PkgConfig_FOUND)
    message(DEBUG "Could not locate PkgConfig")
    return()
  endif()
  set(PN "PC_${name}")
  set(ARGS_PKGCONFIG ${PN} ${ARGS_REQUIRED})
  # if(NOT ARGS_REQUIRED)
  #   list(APPEND ARGS_PKGCONFIG QUIET)
  # endif()
  # if(ARGS_IMPORTED_TARGET)
  #   list(APPEND ARGS_PKGCONFIG IMPORTED_TARGET ${ARGS_IMPORTED_TARGET})
  #   if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.13")
  #     list(APPEND ARGS_PKGCONFIG GLOBAL)
  #   endif()
  # endif()
  list(APPEND ARGS_PKGCONFIG ${ARGS_LIBNAMES})
  pkg_search_module(${ARGS_PKGCONFIG})
  if((NOT ${PN}_FOUND) AND ${PN}_STATIC_FOUND)
    # TODO: Finalize this separately & add alias?
    message(DEBUG "Using located static library for ${name}")
    set(PN "${PN}_STATIC")
  endif()
  message(DEBUG "${PN}_FOUND = ${${PN}_FOUND}")
  message(DEBUG "${PN}_INCLUDE_DIRS = ${${PN}_INCLUDE_DIRS}")
  message(DEBUG "${PN}_LIBRARY_DIRS = ${${PN}_LIBRARY_DIRS}")

  if(${PN}_FOUND)
    ## use the hints from above to find where 'lib*' & '*.h' are located
    copy_cmake_variables("${PN}" "${name}")
    if(TARGET PkgConfig::${PN})
      add_library(${name} ALIAS PkgConfig::${PN})
    endif()
    if(${PN}_FOUND AND NOT ${ARGS_FOUND_VAR})
      message(FATAL_ERROR "Error setting variables from PC vars. ${PN}_FOUND set, but ${ARGS_FOUND_VAR} is not (name = ${name})")
    endif()
  endif()
  if(${PN}_INCLUDE_DIRS)
    list(APPEND ARGS_HEADER_PATHS ${${PN}_INCLUDE_DIRS} ${${PN}_INCLUDE_DIRS}/*)
  endif()
  if(${PN}_LIBRARY_DIRS)
    list(APPEND ARGS_PATHS ${${PN}_LIBRARY_DIRS} ${${PN}_LIBRARY_DIRS}/*)
  endif()
  collect_package_arguments(
    FIND_ARGS ARGS "${options}"
    PATHS HEADER_PATHS
  )
  find_package_brute(
    ${name} ${ARGS_UNPARSED_ARGUMENTS} ${FIND_ARGS}
  )
  set(${name}_BRUTE_SEARCH_PERFORMED ON PARENT_SCOPE)
  _propagate_cmake_variables_package()
endfunction()

function(find_implib_from_dll dll VAR)
  get_filename_component(BIN_DIR ${dll} DIRECTORY)
  get_filename_component(PREFIX_DIR ${BIN_DIR} DIRECTORY)
  get_filename_component(BASE_NAME ${dll} NAME_WLE)
  find_library(
    implib
    NAMES ${BASE_NAME}
    PATHS ${PREFIX_DIR}/lib
  )
  if(NOT implib STREQUAL "implib-NOTFOUND")
    set(${VAR} ${implib} PARENT_SCOPE)
  endif()
endfunction()

function(find_dll_from_implib implib VAR)
  get_filename_component(LIB_DIR ${implib} DIRECTORY)
  get_filename_component(PREFIX_DIR ${LIB_DIR} DIRECTORY)
  get_filename_component(BASE_NAME ${implib} NAME_WLE)
  find_library(
    dll
    NAMES ${BASE_NAME}
    PATHS ${PREFIX_DIR}/bin
  )
  if(NOT dll STREQUAL "dll-NOTFOUND")
    set(${VAR} ${dll} PARENT_SCOPE)
  endif()
endfunction()

function(check_suffixes filename output)
  set(multiValueArgs INCLUDE IGNORE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(out true)
  if (ARGS_INCLUDE)
    set(out false)
    foreach(suffix IN LISTS ARGS_INCLUDE)
      if (filename MATCHES ".*${suffix}$")
        set(out true)
	break()
      endif()
    endforeach()
  endif()
  if (ARGS_IGNORE)
    foreach(suffix IN LISTS ARGS_IGNORE)
      if (filename MATCHES ".*${suffix}$")
        set(out false)
	break()
      endif()
    endforeach()
  endif()
  set(${output} ${out} PARENT_SCOPE)
endfunction()

function(list_search_directories output)
  set(options NO_DEFAULT_PATH NO_CMAKE_PATH NO_CMAKE_ENVIRONMENT_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_INSTALL_PREFIX NO_CMAKE_SYSTEM_PATH NO_PACKAGE_ROOT_PATH)
  set(oneValueArgs FILETYPE)
  set(multiValueArgs PATHS HINTS PATH_SUFFIXES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(${output})
  if (CMAKE_FIND_USE_CMAKE_PATH STREQUAL "FALSE")
    set(ARGS_NO_CMAKE_PATH true)
  endif()
  if (CMAKE_FIND_USE_CMAKE_ENVIRONMENT_PATH STREQUAL "FALSE")
    set(ARGS_NO_CMAKE_ENVIRONMENT_PATH true)
  endif()
  if (CMAKE_FIND_USE_SYSTEM_ENVIRONMENT_PATH STREQUAL "FALSE")
    set(ARGS_NO_SYSTEM_ENVIRONMENT_PATH true)
  endif()
  if (CMAKE_FIND_USE_INSTALL_PREFIX STREQUAL "FALSE")
    set(ARGS_NO_CMAKE_INSTALL_PREFIX true)
  endif()
  if (CMAKE_FIND_USE_CMAKE_SYSTEM_PATH STREQUAL "FALSE")
    set(ARGS_NO_CMAKE_SYSTEM_PATH true)
  endif()
  if (CMAKE_FIND_USE_PACKAGE_ROOT_PATH STREQUAL "FALSE")
    set(ARGS_NO_PACKAGE_ROOT_PATH true)
  endif()
  if (ARGS_FILETYPE STREQUAL "SHARED")
    if (WIN32)
      set(suffix "bin")
    else()
      set(suffix "lib")
    endif()
    set(pathvar "CMAKE_LIBRARY_PATH")
    set(syspathvar "CMAKE_SYSTEM_LIBRARY_PATH")
    set(envvar "LIB")
  elseif (ARGS_FILETYPE STREQUAL "STATIC" OR
          ARGS_FILETYPE STREQUAL "IMPORT")
    set(suffix "lib")
    set(pathvar "CMAKE_LIBRARY_PATH")
    set(syspathvar "CMAKE_SYSTEM_LIBRARY_PATH")
    set(envvar "LIB")
  else()
    set(suffix "include")
    set(pathvar "CMAKE_INCLUDE_PATH")
    set(syspathvar "CMAKE_SYSTEM_INCLUDE_PATH")
    set(envvar "INCLUDE")
  endif()
  # 1. Package root
  # if (NOT (ARGS_NO_DEFAULT_PATH OR ARGS_NO_PACKAGE_ROOT_PATH))
  # endif()
  # 2. & 3. cmake variables
  if (NOT (ARGS_NO_DEFAULT_PATH OR (ARGS_NO_CMAKE_PATH AND ARGS_NO_CMAKE_ENVIRONMENT_PATH)))
    if (DEFINED ENV{CONDA_PREFIX})
      set(mingw64 "$ENV{CONDA_PREFIX}")
      set(mingw32 "$ENV{CONDA_PREFIX}")
      cmake_path(APPEND mingw64
                 "$ENV{CONDA_PREFIX}" "Library" "mingw-w64")
      cmake_path(APPEND mingw32 ${mingw64} "x86_64-w64-mingw32")
      list(APPEND conda_prefixes ${mingw32} ${mingw64} $ENV{CONDA_PREFIX})
      foreach(prefix IN LISTS conda_prefixes)
        if ((EXISTS ${prefix}) AND (NOT ${prefix} IN_LIST CMAKE_PREFIX_PATH))
	  list(INSERT CMAKE_PREFIX_PATH 0 ${prefix})
	endif()
      endforeach()
      if (NOT $ENV{CONDA_PREFIX} IN_LIST CMAKE_PREFIX_PATH)
        list(INSERT CMAKE_PREFIX_PATH 0 $ENV{CONDA_PREFIX})
      endif()
    endif()
    foreach(prefix IN LISTS CMAKE_PREFIX_PATH)
      if (NOT (${prefix} IN_LIST CMAKE_IGNORE_PREFIX_PATH) OR
              (${prefix} IN_LIST CMAKE_SYSTEM_IGNORE_PREFIX_PATH))
        if (CMAKE_LIBRARY_ARCHITECTURE)
          cmake_path(APPEND prefix ${suffix}
  	             ${CMAKE_LIBRARY_ARCHITECTURE} OUTPUT_VARIABLE tmp)
  	  if (NOT (${tmp} IN_LIST CMAKE_IGNORE_PATH) OR
	          (${tmp} IN_LIST CMAKE_SYSTEM_IGNORE_PATH))
            list(APPEND ${output} ${tmp})
	  endif()
        endif()
        cmake_path(APPEND prefix ${suffix} OUTPUT_VARIABLE tmp)
	if (NOT (${tmp} IN_LIST CMAKE_IGNORE_PATH) OR
	        (${tmp} IN_LIST CMAKE_SYSTEM_IGNORE_PATH))
          list(APPEND ${output} ${tmp})
	endif()
      endif()
    endforeach()
    foreach(tmp IN LISTS ${pathvar} CMAKE_FRAMEWORK_PATH)
      if (NOT (${tmp} IN_LIST CMAKE_IGNORE_PATH) OR
              (${tmp} IN_LIST CMAKE_SYSTEM_IGNORE_PATH))
        list(APPEND ${output} ${tmp})
      endif()
    endforeach()
  endif()
  # 4. User specified hints
  list(APPEND ${output} ${ARGS_HINTS})
  # 5. Environment variables
  if (NOT (ARGS_NO_DEFAULT_PATH OR ARGS_NO_SYSTEM_ENVIRONMENT_PATH))
    if (DEFINED ENV{${envvar}})
      string(REPLACE ":" ";" ENVVAR_LIST $ENV{${envvar}})
    endif()
    if (DEFINED ENV{PATH})
      string(REPLACE ":" ";" PATH_LIST $ENV{PATH})
    endif()
    foreach(tmp IN LISTS ENVVAR_LIST PATH_LIST)
      if (NOT (${tmp} IN_LIST CMAKE_IGNORE_PATH) OR
              (${tmp} IN_LIST CMAKE_SYSTEM_IGNORE_PATH))
	list(APPEND ${output} ${tmp})
      endif()
    endforeach()
  endif()
  # Behavior removed in CMake 3.28
  # Search <prefix>/${suffix} and <prefix>/${suffix}/<arch> for each <prefix>/[s]bin in PATH
  # 6. Variables in platform files
  if (NOT (ARGS_NO_DEFAULT_PATH OR ARGS_NO_CMAKE_SYSTEM_PATH))
    foreach(prefix IN LISTS CMAKE_SYSTEM_PREFIX_PATH)
      if (NOT (${prefix} IN_LIST CMAKE_IGNORE_PREFIX_PATH) OR
              (${prefix} IN_LIST CMAKE_SYSTEM_IGNORE_PREFIX_PATH))
        if (CMAKE_LIBRARY_ARCHITECTURE)
          cmake_path(APPEND prefix ${suffix}
	             ${CMAKE_LIBRARY_ARCHITECTURE} OUTPUT_VARIABLE tmp)
          if (NOT (${tmp} IN_LIST CMAKE_IGNORE_PATH) OR
                  (${tmp} IN_LIST CMAKE_SYSTEM_IGNORE_PATH))
            list(APPEND ${output} ${tmp})
	  endif()
        endif()
        cmake_path(APPEND prefix ${suffix} OUTPUT_VARIABLE tmp)
        if (NOT (${tmp} IN_LIST CMAKE_IGNORE_PATH) OR
                (${tmp} IN_LIST CMAKE_SYSTEM_IGNORE_PATH))
          list(APPEND ${output} ${tmp})
	endif()
      endif()
    endforeach()
    foreach(tmp IN LISTS ${syspathvar} CMAKE_SYSTEM_FRAMEWORK_PATH)
      if (NOT (${tmp} IN_LIST CMAKE_IGNORE_PATH) OR
              (${tmp} IN_LIST CMAKE_SYSTEM_IGNORE_PATH))
        list(APPEND ${output} ${tmp})
      endif()
    endforeach()
    if (NOT ARGS_NO_CMAKE_INSTALL_PREFIX)
      foreach(prefix IN LISTS CMAKE_INSTALL_PREFIX CMAKE_STAGING_PREFIX)
        if (NOT (${prefix} IN_LIST CMAKE_IGNORE_PREFIX_PATH) OR
                (${prefix} IN_LIST CMAKE_SYSTEM_IGNORE_PREFIX_PATH))
          list(APPEND ${output} ${prefix})
	endif()
      endforeach()
    endif()
  endif()
  # 7. User specified paths
  list(APPEND ${output} ${ARGS_PATHS})
  # PATH_SUFFIXES
  if (ARGS_PATH_SUFFIXES)
    foreach(dir IN LISTS ${output})
      list(APPEND new_output ${dir})
      foreach(suffix IN LISTS ${ARGS_PATH_SUFFIXES})
        cmake_path(APPEND dir ${suffix} OUTPUT_VARIABLE tmp)
        list(APPEND new_output ${tmp})
      endforeach()
    endforeach()
    set(${output} ${new_output})
  endif()
  list(REMOVE_DUPLICATES ${output})
  set(${output} ${${output}} PARENT_SCOPE)
endfunction()

function(find_library_suffix output libname libtype)
  set(options NO_CACHE REQUIRED)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(${output} AND NOT ARGS_NO_CACHE)
    return()
  endif()
  if (NOT libtype STREQUAL "SHARED")
    find_library(${output} ${libname} ${ARGS_UNPARSED_ARGUMENTS} NO_CACHE)
    message(DEBUG "find_library_suffix: Trying find_library ${libname}")
  else()
    set(${output} "${output}-NOTFOUND")
  endif()
  if (${${output}} STREQUAL "${output}-NOTFOUND")
    set(${output})
    set(prefix)
    set(suffix)
    if (libtype STREQUAL "SHARED" OR
        libtype STREQUAL "STATIC" OR
        libtype STREQUAL "IMPORT")
      set(prefix ${CMAKE_${libtype}_LIBRARY_PREFIX})
      set(suffix ${CMAKE_${libtype}_LIBRARY_SUFFIX})
    elseif (libtype STREQUAL "OBJECT")
      set(prefix)
      set(suffix ${CMAKE_C_OUTPUT_EXTENSION})
    else()
      message(FATAL_ERROR "Invalid libtype ${libtype}")
    endif()
    message(DEBUG "find_library_suffix: Trying find_file ${prefix}${libname}${suffix}")
    find_file(${output} "${prefix}${libname}${suffix}" ${ARGS_UNPARSED_ARGUMENTS} NO_CACHE)
  endif()
  if (${${output}} STREQUAL "${output}-NOTFOUND")
    set(${output})
    message(DEBUG "find_library_suffix: Trying find_file_regex ${prefix}${libname}*${suffix}")
    find_file_regex(${output} "${prefix}${libname}*${suffix}"
                    FILETYPE ${libtype} ${ARGS_UNPARSED_ARGUMENTS}
		    NO_CACHE)
  endif()
  if (${output} STREQUAL "${output}-NOTFOUND")
    if (ARGS_REQUIRED)
      message(FATAL_ERROR "Could not locate a library file for ${libname}")
    endif()
  else()
    message(DEBUG "find_library_suffix: Found library file for ${libname}: ${${output}}")
  endif()
  if (NOT ARGS_NO_CACHE)
    set(${output} "${${output}}" CACHE FILEPATH "The location of the library file for ${libname}")
  else()
    set(${output} "${${output}}" PARENT_SCOPE)
  endif()
endfunction()

function(find_file_regex output pattern)
  set(options NO_CACHE REQUIRED)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(${output} AND NOT ARGS_NO_CACHE)
    return()
  endif()
  list_search_directories(search_dirs ${ARGS_UNPARSED_ARGUMENTS})
  foreach(dir IN LISTS search_dirs)
    cmake_path(APPEND dir ${pattern} OUTPUT_VARIABLE iregex)
    file(GLOB matches LIST_DIRECTORIES false ${iregex})
    foreach(match IN LISTS matches)
      set(${output} ${match})
      break()
    endforeach()
    if (${output})
      break()
    endif()
  endforeach()
  if (NOT ${output})
    set(${output} "${output}-NOTFOUND")
    if (ARGS_REQUIRED)
      message(FATAL_ERROR "Could not locate a file matching the pattern ${pattern}")
    endif()
  else()
  endif()
  if (NOT ARGS_NO_CACHE)
    set(${output} "${${output}}" CACHE FILEPATH "The location of the file matching the regex ${pattern}")
  else()
    set(${output} "${${output}}" PARENT_SCOPE)
  endif()
endfunction()

function(find_libraries)
  set(multiValueArgs LIBRARIES DIRECTORIES
      INCLUDE_SUFFIXES IGNORE_SUFFIXES)
  set(oneValueArgs OUTPUT MISSING MISMATCHED_SUFFIX LIBTYPE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_LIBTYPE)
    if (WIN32)
      set(ARGS_LIBTYPE IMPORT)
    else()
      set(ARGS_LIBTYPE SHARED)
    endif()
  endif()
  list(APPEND ARGS_LIBRARIES ${ARGS_UNPARSED_ARGUMENTS})
  set(LIBS_FULL)
  set(LIBS_MISS)
  set(LIBS_UNMT)
  foreach(x IN LISTS ARGS_LIBRARIES)
    find_library_suffix(${x}_FOUND ${x} ${ARGS_LIBTYPE}
                        PATHS ${ARGS_DIRECTORIES})
    if(${x}_FOUND STREQUAL "${x}_FOUND-NOTFOUND")
      list(APPEND LIBS_MISS ${${x}_FOUND})
    else()
      message(STATUS "Looking for ${x}: ${x}_FOUND = ${${x}_FOUND}")
      check_suffixes(${${x}_FOUND} add_x
                     INCLUDE ${ARGS_INCLUDE_SUFFIXES}
		     IGNORE ${ARGS_IGNORE_SUFFIXES})
      if (add_x)
        list(APPEND LIBS_FULL ${${x}_FOUND})
      else()
        message(DEBUG "Suffix does not match (include=${ARGS_INCLUDE_SUFFIXES}, ignore=${ARGS_IGNORE_SUFFIXES})")
        list(APPEND LIBS_UNMT ${x})
      endif()
    endif()
  endforeach()
  if (ARGS_OUTPUT)
    set(${ARGS_OUTPUT} ${LIBS_FULL} PARENT_SCOPE)
  endif()
  if (ARGS_MISSING)
    set(${ARGS_MISSING} ${LIBS_MISS} PARENT_SCOPE)
  endif()
  if (ARGS_MISMATCHED_SUFFIX)
    set(${ARGS_MISMATCHED_SUFFIX} ${LIBS_UNMT} PARENT_SCOPE)
  endif()
endfunction()
