# https://stackoverflow.com/questions/32183975/how-to-print-all-the-properties-of-a-target-in-cmake
# Get all propreties that cmake supports
if(NOT CMAKE_PROPERTY_LIST)
  execute_process(
    COMMAND cmake --help-property-list
    OUTPUT_VARIABLE CMAKE_PROPERTY_LIST
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  # Convert command output into a CMake list
  string(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
  string(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
  list(REMOVE_DUPLICATES CMAKE_PROPERTY_LIST)
endif()

function(dump_cmake_variables)
  set(options VERBOSE)
  set(oneValueArgs REGEX PREFIX OUTPUT_VAR LOG_LEVEL)
  set(multiValueArgs VARIABLES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_LOG_LEVEL)
    set(ARGS_LOG_LEVEL STATUS)
  endif()
  if(ARGS_VARIABLES)
    set(_variableNames ${ARGS_VARIABLES})
  else()
    get_cmake_property(_variableNames VARIABLES)
  endif()
  list (SORT _variableNames)
  if(NOT ARGS_OUTPUT_VAR)
    set(ARGS_VERBOSE ON)
  endif()
  if(ARGS_VERBOSE)
    set(ARGS_LOG_LEVEL STATUS)
  endif()
  if(ARGS_PREFIX)
    if(ARGS_REGEX)
      message(FATAL_ERROR "Both PREFIX and REGEX provided")
    endif()
    set(ARGS_REGEX "^${ARGS_PREFIX}*")
  endif()
  if (ARGS_VERBOSE)
    message(${ARGS_LOG_LEVEL} "Variables matching \"${ARGS_REGEX}\":")
  endif()
  foreach (_variableName ${_variableNames})
    if (ARGS_REGEX)
      unset(MATCHED)
      string(REGEX MATCH ${ARGS_REGEX} MATCHED ${_variableName})
      if (NOT MATCHED)
        continue()
      endif()
    endif()
    if (ARGS_VERBOSE)
      message(${ARGS_LOG_LEVEL} "    ${_variableName}=${${_variableName}}")
    endif()
    if (ARGS_OUTPUT_VAR)
      list(APPEND ${ARGS_OUTPUT_VAR} ${_variableName})
    endif()
  endforeach()
  if (ARGS_OUTPUT_VAR)
    set(${ARGS_OUTPUT_VAR} ${${ARGS_OUTPUT_VAR}} PARENT_SCOPE)
  endif()
endfunction()

macro(propagate_cmake_variables)
  set(_temp_package_vars "${ARGN}")
  foreach (_variableName IN LISTS _temp_package_vars)
    set(${_variableName} ${${_variableName}} PARENT_SCOPE)
  endforeach()
endmacro()

macro(propagate_cmake_variables_regex NAME)
  set(_temp_package_vars "${ARGN}")
  dump_cmake_variables(
    REGEX ${NAME} OUTPUT_VAR _temp_package_vars
  )
  foreach (_variableName IN LISTS _temp_package_vars)
    set(${_variableName} ${${_variableName}} PARENT_SCOPE)
  endforeach()
endmacro()

macro(propagate_cmake_variables_prefix NAME)
  set(_temp_package_vars "${ARGN}")
  dump_cmake_variables(
    REGEX "^${NAME}*" OUTPUT_VAR _temp_package_vars
  )
  foreach (_variableName IN LISTS _temp_package_vars)
    set(${_variableName} ${${_variableName}} PARENT_SCOPE)
  endforeach()
endmacro()

function(copy_cmake_variables src_prefix dst_prefix)
  dump_cmake_variables(PREFIX "${src_prefix}" OUTPUT_VAR SRC_VARS)
  foreach(srcvar IN LISTS SRC_VARS)
    string(REPLACE "${src_prefix}" "${dst_prefix}" dstvar "${srcvar}")
    set(${dstvar} "${${srcvar}}")
  endforeach()
  propagate_cmake_variables_prefix("${dst_prefix}")
endfunction()

function(check_set PREFIX REASON)
  foreach(ivar ${ARGN})
    if(NOT ${PREFIX}_${ivar})
      message(FATAL_ERROR "Argument \"${ivar}\" required${REASON}")
    endif()
  endforeach()
endfunction()

function(check_not_set PREFIX REASON)
  foreach(ivar ${ARGN})
    if(${PREFIX}_${ivar})
      message(FATAL_ERROR "Argument \"${ivar}\" not allowed${REASON}")
    endif()
  endforeach()
endfunction()

function(check_exclusive_options PREFIX)
  set(options REQUIRED)
  set(oneValueArgs OUTPUT)
  set(multiValueArgs NAMES)
  cmake_parse_arguments(EXCL "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(EXCL_UNPARSED_ARGUMENTS)
    list(APPEND EXCL_NAMES EXCL_UNPARSED_ARGUMENTS)
  endif()
  collect_arguments(
    SET_NAMES "${PREFIX}" "${EXCL_NAMES}"
    ${EXCL_NAMES}
  )
  list(LENGTH SET_NAMES NSET)
  if(NSET EQUAL 0)
    if(EXCL_REQUIRED)
      message(FATAL_ERROR "No option set from ${EXCL_NAMES}")
    endif()
  elseif(NSET EQUAL 1)
    if(EXCL_OUTPUT)
      set(${EXCL_OUTPUT} "${SET_NAMES}" PARENT_SCOPE)
    endif()
  else()
    message(FATAL_ERROR "Conflicting options provided: ${SET_NAMES}")
  endif()
endfunction()

function(set_default var)
  if(NOT ${var})
    set(${var} "${ARGN}" PARENT_SCOPE)
  endif()
endfunction()

function(cmake_parse_arguments_group PREFIX options oneValueArgs multiValueArgs groupArgs)
  cmake_parse_arguments(${PREFIX} "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  foreach(iarg IN LISTS options)
    if(${PREFIX}_${iarg})
      set(${PREFIX}_${iarg} "${iarg}")
    else()
      set(${PREFIX}_${iarg})
    endif()
  endforeach()
  set(${PREFIX}_GROUPED_ARGUMENTS)
  foreach(iarg IN LISTS groupArgs)
    if(${PREFIX}_${iarg})
      list(FIND options "${iarg}" iarg_IDX)
      if(iarg_IDX EQUAL -1)
        list(APPEND ${PREFIX}_GROUPED_ARGUMENTS ${iarg} ${${PREFIX}_${iarg}})
      else()
        list(APPEND ${PREFIX}_GROUPED_ARGUMENTS ${iarg})
      endif()
    endif()
  endforeach()
  propagate_cmake_variables_prefix("${PREFIX}")
endfunction()

function(set_options_to_names PREFIX options)
  foreach(iarg IN LISTS options)
    if(${PREFIX}_${iarg})
      set(${PREFIX}_${iarg} "${iarg}" PARENT_SCOPE)
    else()
      set(${PREFIX}_${iarg} "" PARENT_SCOPE)
    endif()
  endforeach()
endfunction()

function(collect_arguments VAR PREFIX options)
  set(out)
  foreach(iarg IN LISTS ARGN)
    if(${PREFIX}_${iarg})
      list(FIND options "${iarg}" iarg_IDX)
      if(iarg_IDX EQUAL -1)
        list(APPEND out ${iarg} ${${PREFIX}_${iarg}})
      else()
        list(APPEND out ${iarg})
      endif()
    endif()
  endforeach()
  set(${VAR} "${out}" PARENT_SCOPE)
endfunction()

function(check_no_unparsed PREFIX)
  if(${PREFIX}_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unparsed arguments: ${${PREFIX}_UNPARSED_ARGUMENTS}")
  endif()
endfunction()

function(list_append_with_prefix VAR PREFIX)
  foreach(ivar IN LISTS ARGN)
    list(APPEND ${VAR} ${PREFIX}${ivar})
  endforeach()
  set(${VAR} "${${VAR}}" PARENT_SCOPE)
endfunction()

function(list_intersection VAR)
  list(GET ARGN 0 first)
  list(SUBLIST ARGN 1 -1 OTHER_LISTS)
  set(out)
  foreach(ivar IN LISTS ${first})
    foreach(other IN LISTS OTHER_LISTS)
      list(FIND ${other} ${ivar} other_IDX)
      if(other_IDX EQUAL -1)
        break()
      endif()
    endforeach()
    if(NOT other_IDX EQUAL -1)
      list(APPEND out ${ivar})
    endif()
  endforeach()
  set(${VAR} "${out}" PARENT_SCOPE)
endfunction()

function(escape_quotes VAR)
  set(oneValueArgs OUTPUT_VAR)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_OUTPUT_VAR)
    set(ARGS_OUTPUT_VAR ${VAR})
  endif()
  
  set(${ARGS_OUTPUT_VAR} ${${VAR}} PARENT_SCOPE)
endfunction()

function(restore_spaces VAR)
  set(oneValueArgs OUTPUT_VAR)
  cmake_parse_arguments(_ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT _ARGS_OUTPUT_VAR)
    set(_ARGS_OUTPUT_VAR ${VAR})
  endif()
  set(PLACEHOLDER "~SPACE~")
  string(REPLACE "${PLACEHOLDER}" " " out "${${VAR}}")
  set(${_ARGS_OUTPUT_VAR} ${out} PARENT_SCOPE)
endfunction()

function(protect_spaces VAR)
  set(oneValueArgs OUTPUT_VAR)
  cmake_parse_arguments(_ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT _ARGS_OUTPUT_VAR)
    set(_ARGS_OUTPUT_VAR ${VAR})
  endif()
  set(PLACEHOLDER "~SPACE~")
  string(REPLACE " " "${PLACEHOLDER}" out "${${VAR}}")
  set(${_ARGS_OUTPUT_VAR} ${out} PARENT_SCOPE)
endfunction()

function(get_path_sep VAR)
  if(WIN32)
    set(${VAR} "\;" PARENT_SCOPE)
  else()
    set(${VAR} "\:" PARENT_SCOPE)
  endif()
endfunction()

function(extension2language EXT VAR)
  if(EXT STREQUAL ".c")
    set(${VAR} C PARENT_SCOPE)
  elseif(EXT STREQUAL ".cpp")
    set(${VAR} CXX PARENT_SCOPE)
  elseif(EXT STREQUAL ".jl")
    set(${VAR} Julia PARENT_SCOPE)
  elseif(EXT STREQUAL ".py")
    set(${VAR} Python PARENT_SCOPE)
  elseif(EXT STREQUAL ".R")
    set(${VAR} R PARENT_SCOPE)
  elseif(EXT STREQUAL ".m")
    set(${VAR} Matlab PARENT_SCOPE)
  else()
    string(REGEX MATCH "[.][fF]((90)|(95)|(03)|(08)|(18))?$" match ${EXT})
    if(match)
      set(${VAR} Fortran PARENT_SCOPE)
    else()
      message(FATAL_ERROR "Support for extension \"${EXT}\" not implemented")
    endif()
  endif()
endfunction()

function(language2compilerenv language VAR)
  if(language STREQUAL C)
    set(${VAR} "CC" PARENT_SCOPE)
  elseif(language STREQUAL CXX)
    set(${VAR} "CXX" PARENT_SCOPE)
  elseif(language STREQUAL Fortran)
    set(${VAR} "FC" PARENT_SCOPE)
  else()
    message(FATAL_ERROR "Support for language \"${language}\" not implemented")
  endif()
endfunction()

function(language2srcext language VAR)
  if(language STREQUAL C)
    set(${VAR} ".c" PARENT_SCOPE)
  elseif(language STREQUAL CXX)
    set(${VAR} ".cpp" PARENT_SCOPE)
  elseif(language STREQUAL Fortran)
    set(${VAR} ".F90" PARENT_SCOPE)
  elseif(language STREQUAL Julia)
    set(${VAR} ".jl" PARENT_SCOPE)
  elseif(language STREQUAL Python)
    set(${VAR} ".py" PARENT_SCOPE)
  elseif(language STREQUAL R)
    set(${VAR} ".R" PARENT_SCOPE)
  elseif(language STREQUAL Matlab)
    set(${VAR} ".m" PARENT_SCOPE)
  else()
    message(ERROR "Support for language \"${language}\" not implemented")
  endif()
endfunction()

function(file2language TARGET VAR)
  cmake_path(GET TARGET EXTENSION TARGET_EXT)
  extension2language(${TARGET_EXT} ${VAR})
  set(${VAR} ${${VAR}} PARENT_SCOPE)
endfunction()

function(get_supported_generators VAR)
  execute_process(
    COMMAND python ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/scripts/check_generators.py
    OUTPUT_VARIABLE RAW_OUTPUT
    COMMAND_ERROR_IS_FATAL ANY
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  set(output "${CMAKE_GENERATOR}")
  if(WIN32)
    list(APPEND output "MinGW Makefiles")
  endif()
  list(APPEND output ${RAW_OUTPUT})
  # foreach(iout IN LISTS RAW_OUTPUT)
  #   if(NOT iout STREQUAL "${CMAKE_GENERATOR}")
  #     list(APPEND output ${iout})
  #   endif()
  # endforeach()
  list(REMOVE_DUPLICATES output)
  set(${VAR} ${output} PARENT_SCOPE)
endfunction()

function(get_supported_languages VAR)
  set(options COMPILED)
  set(multiValueArgs EXCLUDE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_COMPILED)
    set(out C CXX Fortran)
  else()
    set(out C CXX Fortran Julia Python R Matlab)
  endif()
  if(ARGS_EXCLUDE)
    foreach(ilanguage IN LISTS out)
      list(FIND ARGS_EXCLUDE "${ilanguage}" ilanguage_IDX)
      if(ilanguage_IDX EQUAL -1)
        list(APPEND ${VAR} ${ilanguage})
      endif()
    endforeach()
  else()
    list(APPEND ${VAR} ${out})
  endif()
  set(${VAR} ${${VAR}} PARENT_SCOPE)
endfunction()

function(show_build_info)
  set(oneValueArgs PREFIX LOG_LEVEL)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_LOG_LEVEL)
    set(ARGS_LOG_LEVEL DEBUG)
  endif()
  list(APPEND ARGS_UNPARSED_ARGUMENTS LOG_LEVEL ${ARGS_LOG_LEVEL})
  if(ARGS_PREFIX)
    list(APPEND ARGS_UNPARSED_ARGUMENTS PREFIX ${ARGS_PREFIX})
  endif()
  set(
    BASE_ARGUMENTS
    CMAKE_GENERATOR
    CMAKE_VERBOSE_MAKEFILE
    CMAKE_MESSAGE_LOG_LEVEL
    CMAKE_GNUtoMS
    MSVC_AND_GNU_BUILD
  )
  foreach(iarg IN LISTS BASE_ARGUMENTS)
    message(${ARGS_LOG_LEVEL} "${ARGS_PREFIX}${iarg} = ${${iarg}}")
  endforeach()
  show_compiler_env_vars(${ARGS_UNPARSED_ARGUMENTS})
  show_compilers(${ARGS_UNPARSED_ARGUMENTS})
  show_implicit_libraries(${ARGS_UNPARSED_ARGUMENTS})
endfunction()

function(show_compiler_env_vars)
  set(oneValueArgs PREFIX LOG_LEVEL)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_LOG_LEVEL)
    set(ARGS_LOG_LEVEL DEBUG)
  endif()
  get_supported_languages(COMPILED_LANGUAGES COMPILED)
  foreach(ilanguage IN LISTS COMPILED_LANGUAGES)
    language2compilerenv(${ilanguage} ienv)
    message(${ARGS_LOG_LEVEL} "${ARGS_PREFIX}${ienv} = $ENV{${ienv}}")
  endforeach()
endfunction()

function(show_implicit_libraries)
  set(oneValueArgs PREFIX LOG_LEVEL)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_LOG_LEVEL)
    set(ARGS_LOG_LEVEL DEBUG)
  endif()
  get_supported_languages(COMPILED_LANGUAGES COMPILED)
  foreach(ilanguage IN LISTS COMPILED_LANGUAGES)
    message(${ARGS_LOG_LEVEL} "${ARGS_PREFIX}CMAKE_${ilanguage}_IMPLICIT_LINK_LIBRARIES = ${CMAKE_${ilanguage}_IMPLICIT_LINK_LIBRARIES}")
  endforeach()
endfunction()

function(show_compilers)
  set(oneValueArgs PREFIX LOG_LEVEL)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_LOG_LEVEL)
    set(ARGS_LOG_LEVEL DEBUG)
  endif()
  get_supported_languages(COMPILED_LANGUAGES COMPILED)
  foreach(ilanguage IN LISTS COMPILED_LANGUAGES)
    message(${ARGS_LOG_LEVEL} "${ARGS_PREFIX}CMAKE_${ilanguage}_COMPILER = ${CMAKE_${ilanguage}_COMPILER}")
  endforeach()
endfunction()

function(sort_files_by_language prefix)
  set(oneValueArgs OUTPUT_LANGUAGES)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_SOURCES)
    set(ARGS_SOURCES ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  get_supported_languages(LANGUAGES)
  foreach(ilanguage IN LISTS LANGUAGES)
    set(${prefix}_${ilanguage})
  endforeach()
  foreach(ifile IN LISTS ARGS_SOURCES)
    set(ilanguage)
    file2language(${ifile} ilanguage)
    list(APPEND ${prefix}_${ilanguage} ${ifile})
  endforeach()
  set(source_languages)
  foreach(ilanguage IN LISTS LANGUAGES)
    set(${prefix}_${ilanguage} ${${prefix}_${ilanguage}} PARENT_SCOPE)
    if(${prefix}_${ilanguage})
      list(APPEND source_languages ${ilanguage})
    endif()
  endforeach()
  if(ARGS_OUTPUT_LANGUAGES)
    set(${ARGS_OUTPUT_LANGUAGES} ${source_languages} PARENT_SCOPE)
  endif()
endfunction()

function(select_files_by_language language destination)
  get_supported_languages(LANGUAGES)
  set(oneValueArgs OTHER_DESTINATION)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_SOURCES)
    set(ARGS_SOURCES ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  sort_files_by_language(SRC SOURCES ${ARGS_SOURCES})
  set(sources)
  set(other_sources)
  foreach(ilanguage IN LISTS LANGUAGES)
    if(ilanguage STREQUAL "${language}")
      list(APPEND sources ${SRC_${ilanguage}})
    else()
      list(APPEND other_sources ${SRC_${ilanguage}})
    endif()
  endforeach()
  set(${destination} ${sources} PARENT_SCOPE)
  if(ARGS_OTHER_DESTINATION)
    set(${ARGS_OTHER_DESTINATION} ${other_sources} PARENT_SCOPE)
  endif()
endfunction()

function(inspect_target name)
  set(oneValueArgs LOG_LEVEL)
  set(multiValueArgs PROPERTIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT TARGET ${name})
    message(FATAL_ERROR "\"${name}\" is not a target")
  endif()
  set_default(ARGS_LOG_LEVEL STATUS)
  if(NOT ARGS_PROPERTIES)
    list(
      APPEND ARGS_PROPERTIES TYPE IMPORTED
      ${CMAKE_PROPERTY_LIST}
    )
    list(REMOVE_DUPLICATES ARGS_PROPERTIES)
  endif()
  message(${ARGS_LOG_LEVEL} "Inspecting target \"${name}\"")
  foreach(property IN LISTS ARGS_PROPERTIES)
    string(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" property ${property})
    # if(property STREQUAL "LOCATION" OR property MATCHES "^LOCATION_" OR property MATCHES "_LOCATION$")
    #   continue()
    # endif()
    
    if(property MATCHES "<LANG>")
      continue()
    else()
      get_property(was_set TARGET ${name} PROPERTY ${property} SET)
      if(was_set)
        get_target_property(value ${name} ${property})
        message(${ARGS_LOG_LEVEL} "    ${property} = ${value}")
      endif()
    endif()
  endforeach()
endfunction()

function(set_environment_vars)
  set(oneValueArgs EXISTING KEYPREFIX)
  set(options END_VARS)
  set(multiValueArgs VARS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_VARS AND NOT ARGS_END_VARS)
    message(FATAL_ERROR "If VARS is provided, END_VARS must be as well.")
  endif()
  if (NOT ARGS_KEYPREFIX)
    set(ARGS_KEYPREFIX ENVKEY_)
  endif()
  string(LENGTH "${ARGS_KEYPREFIX}" KEYPREFIX_LENGTH)
  set(existing)
  foreach(x IN LISTS ARGS_VARS)
    if ("${x}" MATCHES "${ARGS_KEYPREFIX}*")
      if(ikey)
        list(APPEND existing "${ARGS_KEYPREFIX}${ikey}" "$ENV{${ikey}}")
	string(REPLACE ";" " " ival "${ival}")
	set(ENV{${ikey}} "${ival}")
      endif()
      string(SUBSTRING "${x}" ${KEYPREFIX_LENGTH} -1 ikey)
      set(ival)
    else()
      list(APPEND ival ${x})
    endif()
  endforeach()
  if(ikey)
    list(APPEND existing "${ARGS_KEYPREFIX}${ikey}" "$ENV{${ikey}}")
    string(REPLACE ";" " " ival "${ival}")
    set(ENV{${ikey}} "${ival}")
  endif()
  if(ARGS_EXISTING)
    set(${ARGS_EXISTING} ${existing} PARENT_SCOPE)
  endif()
endfunction()

function(add_custom_command_function function)
  set(options PRE_BUILD PRE_LINK POST_BUILD
      COMMAND_EXPAND_LISTS USES_TERMINAL)
  set(oneValueArgs MODULE DEST DEST_DIR
      TARGET COMMENT WORKING_DIRECTORY)
  set(multiValueArgs FUNCTION_ARGUMENTS
      PRESERVE_VARIABLES COMMAND_ARGUMENTS
      BYPRODUCTS DEPENDS OUTPUT)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  collect_arguments(
    FUNCTION_ARGS ARGS "${options}"
    MODULE DEST DEST_DIR PRESERVE_VARIABLES
    FUNCTION_ARGUMENTS COMMAND_ARGUMENTS
  )
  collect_arguments(
    COMMAND_ARGS ARGS "${options}"
    COMMAND_EXPAND_LISTS USES_TERMINAL
    COMMENT WORKING_DIRECTORY
    BYPRODUCTS DEPENDS
  )
  setup_external_function(
    ${function}
    OUTPUT_COMMAND CUSTOM_COMMAND
    ${FUNCTION_ARGS}
  )
  list(APPEND COMMAND_ARGS COMMAND ${CUSTOM_COMMAND})
  message(STATUS "CUSTOM_COMMAND = ${CUSTOM_COMMAND}")
  if(ARGS_TARGET)
    check_exclusive_options(
      ARGS REQUIRED OUTPUT ARGS_EVENT
      NAMES PRE_BUILD PRE_LINK POST_BUILD
    )
    check_not_set(ARGS " when TARGET provided" DEPENDS OUTPUT)
    add_custom_command(
      TARGET ${ARGS_TARGET} ${ARGS_EVENT}
      ${COMMAND_ARGS}
    )
  else()
    check_set(ARGS " when TARGET not provided" OUTPUT)
    check_not_set(
      ARGS " when TARGET not provided"
      PRE_BUILD PRE_LINK POST_BUILD
    )
    add_custom_command(
      OUTPUT ${ARGS_OUTPUT}
      ${COMMAND_ARGS}
    )
  endif()
endfunction()

function(setup_external_function function)
  set(options INCLUDE)
  set(oneValueArgs MODULE DEST DEST_DIR OUTPUT_COMMAND)
  set(multiValueArgs ARGUMENTS PRESERVE_VARIABLES COMMAND_ARGUMENTS
      FUNCTION_ARGUMENTS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  list(APPEND ARGS_PRESERVE_VARIABLES
       CMAKE_VERBOSE_MAKEFILE CMAKE_MESSAGE_LOG_LEVEL)
  if(ARGS_FUNCTION_ARGUMENTS)
    list(APPEND ARGS_ARGUMENTS ${ARGS_FUNCTION_ARGUMENTS})
  endif()
  if(ARGS_UNPARSED_ARGUMENTS)
    list(APPEND ARGS_ARGUMENTS ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if(NOT ARGS_DEST_DIR)
    set(ARGS_DEST_DIR ${CMAKE_CURRENT_BINARY_DIR})
  endif()
  if(NOT ARGS_DEST)
    if(NOT (ARGS_INCLUDE OR ARGS_OUTPUT_COMMAND))
      message(FATAL_ERROR "Neither DEST, INCLUDE or OUTPUT_COMMAND set")
    endif()
    cmake_path(
      APPEND ARGS_DEST_DIR "call_${function}.cmake"
      OUTPUT_VARIABLE ARGS_DEST
    )
  endif()
  
  set(ARGS_FUNCTION ${function})
  protect_spaces(ARGS_ARGUMENTS)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/external_function.cmake.in
    ${ARGS_DEST}
    @ONLY
  )
  if(ARGS_INCLUDE)
    include(${ARGS_DEST})
  endif()
  set(OUTPUT_COMMAND ${CMAKE_COMMAND} ${ARGS_COMMAND_ARGUMENTS})
  foreach(var IN LISTS ARGS_PRESERVE_VARIABLES)
    list(APPEND OUTPUT_COMMAND "-D${var}=${${var}}")
  endforeach()
  list(APPEND OUTPUT_COMMAND -P ${ARGS_DEST})
  if(ARGS_OUTPUT_COMMAND)
    set(${ARGS_OUTPUT_COMMAND} ${OUTPUT_COMMAND} PARENT_SCOPE)
  endif()
endfunction()

function(execute_process_generic)
  set(oneValueArgs TARGET OUTPUT_VARIABLE ERROR_VARIABLE RESULT_VARIABLE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  foreach(ivar OUTPUT_VARIABLE ERROR_VARIABLE RESULT_VARIABLE)
    if(ARGS_${ivar})
      list(APPEND ARGS_UNPARSED_ARGUMENTS ${ivar} ${ARGS_${ivar}})
    endif()
  endforeach()
  if(ARGS_TARGET)
    execute_process_as_target(${ARGS_TARGET} ${ARGS_UNPARSED_ARGUMENTS})
  else()
    execute_process_with_env(${ARGS_UNPARSED_ARGUMENTS})
    foreach(ivar OUTPUT_VARIABLE ERROR_VARIABLE RESULT_VARIABLE)
      if(ARGS_${ivar})
        set(${ARGS_${ivar}} ${${ARGS_${ivar}}} PARENT_SCOPE)
      endif()
    endforeach()
  endif()
endfunction()

function(execute_process_as_target target)
  set(options ALL VERBATIM USES_TERMINAL COMMAND_EXPAND_LISTS
      # execute_process_with_env arguments
      ENV_VARS_END)
  set(oneValueArgs OUTPUT
      # execute_process_with_env arguments
      ENV_VAR_PREFIX COMMENT
      TIMEOUT WORKING_DIRECTORY
      OUTPUT_VARIABLE ERROR_VARIABLE RESULT_VARIABLE
      ECHO_OUTPUT_VARIABLE ECHO_ERROR_VARIABLE)
  set(multiValueArgs DEPENDS BYPRODUCTS
      # execute_process_with_env arguments
      COMMAND ENV_VARS ENV_VARS_CLEAR CLEAR_COMPILERS
      PREPEND_PATH APPEND_PATH)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  check_no_unparsed()
  foreach(ivar OUTPUT_VARIABLE ERROR_VARIABLE RESULT_VARIABLE)
    if(ARGS_${ivar})
      message(FATAL_ERROR "\"${var}\" invalid when running process as a custom target (\"${target}\") as the process will not be executed until build time")
      list(APPEND ARGS_UNPARSED_ARGUMENTS ${ivar} ${ARGS_${ivar}})
    endif()
  endforeach()
  if(ARGS_ALL)
    set(ARGS_ALL ALL)
  else()
    set(ARGS_ALL)
  endif()
  collect_arguments(
    PROCESS_ARGS ARGS "${options}"
    COMMENT WORKING_DIRECTORY COMMAND
  )
  collect_arguments(
    EXECUTE_ARGS ARGS "${options}"
    TIMEOUT ECHO_OUTPUT_VARIABLE ECHO_ERROR_VARIABLE
    ENV_VAR_PREFIX ENV_VARS ENV_VARS_CLEAR CLEAR_COMPILERS
    PREPEND_PATH APPEND_PATH
  )
  collect_arguments(
    COMMAND_ARGS ARGS "${options}"
    VERBATIM USES_TERMINAL COMMAND_EXPAND_LISTS
    DEPENDS BYPRODUCTS
  )
  if(EXECUTE_ARGS)
    setup_external_function(
      "execute_process_with_env" MODULE "GeneralTools"
      DEST ${CMAKE_CURRENT_BINARY_DIR}/execute_${target}.cmake
      OUTPUT_COMMAND TARGET_COMMAND
      ARGUMENTS ${PROCESS_ARGS} ${EXECUTE_ARGS}
    )
    list(APPEND COMMAND_ARGS COMMAND ${TARGET_COMMAND})
  else()
    list(APPEND COMMAND_ARGS ${PROCESS_ARGS})
  endif()
  if(ARGS_OUTPUT)
    add_custom_command(
      OUTPUT ${ARGS_OUTPUT}
      ${COMMAND_ARGS}
    )
    add_custom_target(
      ${target} ${ARGS_ALL}
      DEPENDS ${ARGS_OUTPUT}
    )
  else()
    add_custom_target(
      ${target} ${ARGS_ALL}
      ${COMMAND_ARGS}
    )
  endif()
endfunction()

function(execute_process_with_env)
  set(options ENV_VARS_END)
  set(oneValueArgs ENV_VAR_PREFIX RESULT_VARIABLE COMMENT
      TIMEOUT WORKING_DIRECTORY OUTPUT_VARIABLE ERROR_VARIABLE
      ECHO_OUTPUT_VARIABLE ECHO_ERROR_VARIABLE)
  set(multiValueArgs COMMAND ENV_VARS ENV_VARS_CLEAR CLEAR_COMPILERS
      PREPEND_PATH APPEND_PATH)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_COMMAND)
    message(FATAL_ERROR "COMMAND not provided")
  endif()
  if(NOT ARGS_ENV_VAR_PREFIX)
    if(ARGS_ENV_VARS)
      message(FATAL_ERROR "ENV_VARS provided, but no ENV_VAR_PREFIX")
    else()
      set(ARGS_ENV_VAR_PREFIX "ENVK_")
    endif()
  endif()
  if(ARGS_PREPEND_PATH OR ARGS_APPEND_PATH)
    set(UPDATED_PATH "$ENV{PATH}")
  endif()
  get_path_sep(PATH_SEP)
  if(ARGS_PREPEND_PATH)
    string(REPLACE ";" "${PATH_SEP}" ARGS_PREPEND_PATH "${ARGS_PREPEND_PATH}")
    set(UPDATED_PATH "${ARGS_PREPEND_PATH}${PATH_SEP}${UPDATED_PATH}")
  endif()
  if(ARGS_APPEND_PATH)
    string(REPLACE ";" "${PATH_SEP}" ARGS_APPEND_PATH "${ARGS_APPEND_PATH}")
    set(UPDATED_PATH "${UPDATED_PATH}${PATH_SEP}${APPEND_PATH}")
  endif()
  if(UPDATED_PATH)
    list(APPEND ARGS_ENV_VARS "${ARGS_ENV_VAR_PREFIX}PATH" "${UPDATED_PATH}")
  endif()
  foreach(ilanguage IN LISTS ARGS_CLEAR_COMPILERS)
    language2compilerenv(${ilanguage} ienv)
    list(APPEND ARGS_ENV_VARS_CLEAR ${ienv})
  endforeach()
  foreach(var IN LISTS ARGS_ENV_VARS_CLEAR)
    list(APPEND ARGS_ENV_VARS "${ARGS_ENV_VAR_PREFIX}${var}" "")
  endforeach()
  if(ARGS_ENV_VARS)
    message(DEBUG "ENV_VARS = ${ARGS_ENV_VARS}")
    message(DEBUG "ENV_VAR_PREFIX = ${ARGS_ENV_VAR_PREFIX}")
    set_environment_vars(
      VARS ${ARGS_ENV_VARS} END_VARS
      KEYPREFIX ${ARGS_ENV_VAR_PREFIX}
      EXISTING REPLACED_ENV_VARS
    )
    message(DEBUG "REPLACED_ENV_VARS = ${REPLACED_ENV_VARS}")
  endif()
  if(ARGS_COMMENT)
    message(STATUS "execute_process_with_env: Running ${ARGS_COMMENT}")
  endif()
  message(STATUS "execute_process_with_env: ARGS_COMMAND = ${ARGS_COMMAND}")
  collect_arguments(
    PROCESS_ARGS ARGS "${options}"
    TIMEOUT WORKING_DIRECTORY
    OUTPUT_VARIABLE ERROR_VARIABLE
    ECHO_OUTPUT_VARIABLE ECHO_ERROR_VARIABLE
  )
  execute_process(
    COMMAND ${ARGS_COMMAND}
    RESULT_VARIABLE ret
    ${PROCESS_ARGS}
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  if(ARGS_ENV_VARS)
    set_environment_vars(
      VARS ${REPLACED_ENV_VARS} END_VARS
      KEYPREFIX ${ARGS_ENV_VAR_PREFIX}
    )
  endif()
  if(ARGS_RESULT_VARIABLE)
    set(${ARGS_RESULT_VARIABLE} ${ret})
  elseif(NOT ret EQUAL 0)
    message(FATAL_ERROR "Failed to run ${ARGS_COMMAND}")
  endif()
  foreach(ivar OUTPUT_VARIABLE ERROR_VARIABLE RESULT_VARIABLE)
    if(ARGS_${ivar})
      set(${ARGS_${ivar}} ${${ARGS_${ivar}}} PARENT_SCOPE)
    endif()
  endforeach()
endfunction()

