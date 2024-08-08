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


function(get_runtime_environment_var OUTPUT_VARIABLE)
  if(WIN32)
    set(OUT PATH)
  elseif(APPLE)
    set(OUT DYLD_LIBRARY_PATH)
  else()
    set(OUT LD_LIBRARY_PATH)
  endif()
  set(${OUTPUT_VARIABLE} ${OUT} PARENT_SCOPE)
endfunction()


function(get_pathsep OUTPUT_VARIABLE)
  if(WIN32)
    set(OUT "\\;")
  else()
    set(OUT ":")
  endif()
  set(${OUTPUT_VARIABLE} ${OUT} PARENT_SCOPE)
endfunction()


function(update_env_path)
  set(options PREPEND)
  set(oneValueArgs PATH_VARIABLE PATH_SEP OUTPUT_VARIABLE PREVIOUS_VALUE)
  set(multiValueArgs PATHS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_UNPARSED_ARGUMENTS)
    list(APPEND ARGS_PATHS ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if(NOT ARGS_PATHS)
    return()
  endif()
  if(NOT ARGS_PATH_VARIABLE)
    get_runtime_environment_var(ARGS_PATH_VARIABLE)
  endif()
  if(NOT ARGS_PATH_SEP)
    get_pathsep(ARGS_PATH_SEP)
  endif()
  list(JOIN ARGS_PATHS "${ARGS_PATH_SEP}" NEW_PATHS)
  if(NOT DEFINED ENV{${ARGS_PATH_VARIABLE}})
    set(UPDATED_PATHS "${NEW_PATHS}")
  elseif(ARGS_PREPEND)
    set(UPDATED_PATHS "${NEW_PATHS}${ARGS_PATH_SEP}$ENV{${ARGS_PATH_VARIABLE}}")
  else()
    set(UPDATED_PATHS "$ENV{${ARGS_PATH_VARIABLE}}${ARGS_PATH_SEP}${NEW_PATHS}")
  endif()
  if(ARGS_PREVIOUS_VALUE)
    set(${PREVIOUS_VALUE} $ENV{${ARGS_PATH_VARIABLE}} PARENT_SCOPE)
  endif()
  if(ARGS_OUTPUT_VARIABLE)
    # PARENT_SCOPE?
    set(${ARGS_OUTPUT_VARIABLE} "${UPDATED_PATHS}")
    set(${ARGS_OUTPUT_VARIABLE} "${UPDATED_PATHS}" PARENT_SCOPE)
  else()
    set(ENV{${ARGS_PATH_VARIABLE}} "${UPDATED_PATHS}" PARENT_SCOPE)
  endif()
endfunction()


function(get_runtime_directory_suffix OUTPUT_VARIABLE)
  if(WIN32)
    set(OUT "bin")
  else()
    set(OUT "lib")
  endif()
  set(${OUTPUT_VARIABLE} ${OUT} PARENT_SCOPE)
endfunction()


function(configure_and_build CONFIG_FILE)
  set(oneValueArgs SOURCE_DIR BUILD_DIR)
  set(multiValueArgs PREPEND_PATHS CMAKE_COMMAND_ARGS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_SOURCE_DIR)
    set(ARGS_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/_tmp)
  endif()
  if(NOT ARGS_BUILD_DIR)
    set(ARGS_BUILD_DIR ${ARGS_SOURCE_DIR}_build)
  endif()
  cmake_path(APPEND ARGS_SOURCE_DIR "CMakeLists.txt" OUTPUT_VARIABLE OUTPUT_FILE)
  configure_file(${CONFIG_FILE} ${OUTPUT_FILE} @ONLY)
  file(MAKE_DIRECTORY ${ARGS_BUILD_DIR})
  if(ARGS_PREPEND_PATHS)
    update_env_path(
      PREPEND
      PATH_VARIABLE PATH
      PATHS ${ARGS_PREPEND_PATHS}
      PREVIOUS_VALUE old_paths
    )
  endif()
  execute_process(
    COMMAND ${CMAKE_COMMAND} ${ARGS_SOURCE_DIR} ${ARGS_CMAKE_COMMAND_ARGS}
    WORKING_DIRECTORY ${ARGS_BUILD_DIR}
    COMMAND_ECHO STDOUT
    RESULT_VARIABLE ret)
  if (NOT ret EQUAL 0)
    message(FATAL_ERROR "Failed to configure ${OUTPUT_FILE}")
  endif()
  execute_process(
    COMMAND ${CMAKE_COMMAND} --build . --config Release
    WORKING_DIRECTORY ${ARGS_BUILD_DIR}
    COMMAND_ECHO STDOUT
    RESULT_VARIABLE ret)
  if(old_paths)
    set(ENV{PATH} "${old_paths}")
  endif()
  if (NOT ret EQUAL 0)
    message(FATAL_ERROR "Failed to build ${OUTPUT_FILE}")
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
    get_pathsep(PATH_SEP)
    list(JOIN ARGS_PATHS "${PATH_SEP}" NEW_PATHS)
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


function(cmakevar2cmakecliarg opt OUTPUT_LIST_VARIABLE)
  if(NOT ${opt})
    return()
  endif()
  list(LENGTH ${opt} opt_len)
  if (opt_len GREATER 1)
    list(JOIN ${opt} "\\\\;" tmp)
    set(OUT -D${opt}=${tmp})
  else()
    set(OUT -D${opt}=${${opt}})
  endif()
  list(APPEND ${OUTPUT_LIST_VARIABLE} ${OUT})
  set(${OUTPUT_LIST_VARIABLE} ${${OUTPUT_LIST_VARIABLE}} PARENT_SCOPE)
endfunction()


function(cmakevars2cmakecliargs OUTPUT_LIST_VARIABLE)
  foreach(opt ${ARGN})
    cmakevar2cmakecliarg(${opt} ${OUTPUT_LIST_VARIABLE})
  endforeach()
  set(${OUTPUT_LIST_VARIABLE} ${${OUTPUT_LIST_VARIABLE}} PARENT_SCOPE)
endfunction()
