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
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CMakeAddFortranSubdirectory/execute_env.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake.in
    @ONLY)
  if (ARGS_OUTPUT)
    add_custom_command(
      OUTPUT ${ARGS_OUTPUT}
      COMMAND ${CMAKE_COMMAND} ARGS -P ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake.in
      ${ARGS_UNPARSED_ARGUMENTS})
    add_custom_target(
      ${target} ALL
      DEPENDS ${ARGS_OUTPUT})
  else()
    add_custom_target(
      ${target} ALL
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake.in
      ${ARGS_UNPARSED_ARGUMENTS})
  endif()
endfunction()
