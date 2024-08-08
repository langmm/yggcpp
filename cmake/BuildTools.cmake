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
  foreach(x IN LISTS ARGS_VARS)
    if ("${x}" MATCHES "${ARGS_KEYPREFIX}*")
      if(ikey)
        if (ARGS_EXISTING)
	  list(APPEND ARGS_EXISTING "${ARGS_KEYPREFIX}${ikey}"
	       "$ENV{${ikey}}")
	endif()
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
    if (ARGS_EXISTING)
      list(APPEND ARGS_EXISTING "${ARGS_KEYPREFIX}${ikey}" "$ENV{${ikey}}")
    endif()
    string(REPLACE ";" " " ival "${ival}")
    set(ENV{${ikey}} "${ival}")
  endif()
endfunction()

function(add_custom_command_env target)
  set(options END_ENV)
  set(oneValueArgs COMMENT KEYPREFIX)
  set(multiValueArgs COMMAND ENV OUTPUT)  # DEPENDS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_ENV AND NOT ARGS_END_ENV)
    message(FATAL_ERROR "If ENV provided, END_ENV must be as well.")
  endif()
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/execute_env.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake
    @ONLY)
  if (ARGS_OUTPUT)
    add_custom_command(
      OUTPUT ${ARGS_OUTPUT}
      COMMAND ${CMAKE_COMMAND} ARGS -P ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake
      ${ARGS_UNPARSED_ARGUMENTS})
    add_custom_target(
      ${target} ALL
      DEPENDS ${ARGS_OUTPUT})
  else()
    add_custom_target(
      ${target} ALL
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake
      ${ARGS_UNPARSED_ARGUMENTS})
  endif()
endfunction()

function(configure_env_injection)
  set(oneValueArgs OUTPUT_FILE DIRECTORY)
  set(multiValueArgs VARIABLES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_OUTPUT_FILE)
    set(ARGS_OUTPUT_FILE ${CMAKE_CURRENT_BINARY_DIR}/CTestEnvInject.cmake)
  endif()
  if (ARGS_UNPARSED_ARGUMENTS)
    list(APPEND ARGS_VARIABLES ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  set(ENV_VARS ${ARGS_VARIABLES})
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/CTestEnvInject.cmake.in
    ${ARGS_OUTPUT_FILE}
    @ONLY)
  if (ARGS_DIRECTORY)
    set_property(
      DIRECTORY ${ARGS_DIRECTORY} APPEND PROPERTY
      TEST_INCLUDE_FILES ${ARGS_OUTPUT_FILE}
    )
  endif()
endfunction()

function(configure_path_injection)
  set(oneValueArgs OUTPUT_FILE PATH_VARIABLE DIRECTORY)
  set(multiValueArgs PATHS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_OUTPUT_FILE)
    set(ARGS_OUTPUT_FILE ${CMAKE_CURRENT_BINARY_DIR}/CTestPathInject.cmake)
  endif()
  if (NOT ARGS_PATH_VARIABLE)
    set(ARGS_PATH_VARIABLE PATH)
  endif()
  set(PATH_VAR ${ARGS_PATH_VARIABLE})
  if(ARGS_UNPARSED_ARGUMENTS)
    list(APPEND ARGS_PATHS ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if(ARGS_PATHS)
    if(WIN32)
      set(PATH_SEP ";")
    else()
      set(PATH_SEP ":")
    endif()
    string(REPLACE ";" "${PATH_SEP}" NEW_PATHS "${ARGS_PATHS}")
  else()
    set(PATH_SEP)
    set(NEW_PATHS)
  endif()
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/CTestPathInject.cmake.in
    ${ARGS_OUTPUT_FILE}
    @ONLY)
  if (ARGS_DIRECTORY)
    set_property(
      DIRECTORY ${ARGS_DIRECTORY} APPEND PROPERTY
      TEST_INCLUDE_FILES ${ARGS_OUTPUT_FILE}
    )
  endif()
endfunction()
