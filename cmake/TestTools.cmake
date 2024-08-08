function(set_tests_environment TEST_NAME)
  include(BuildTools)
  set(options FOR_GTEST_DISCOVER_TESTS)
  set(oneValueArgs DIRECTORY OUTPUT_PROPERTIES)
  set(multiValueArgs VARIABLES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_VARIABLES)
    return()
  endif()
  if(ARGS_UNPARSED_ARGUMENTS)
    list(APPEND TEST_NAME ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if(NOT ARGS_DIRECTORY)
    set(ARGS_DIRECTORY .)
  endif()
  if(ARGS_FOR_GTEST_DISCOVER_TESTS)
    foreach(var ${ARGS_VARIABLES})
      list(APPEND properties ENVIRONMENT "${var}")
    endforeach()
  else()
    set(properties ${ARGS_VARIABLES})
  endif()
  message(STATUS "Setting \'${TEST_NAME}\' test properties: ${properties}")
  if(ARGS_OUTPUT_PROPERTIES)
    set(${ARGS_OUTPUT_PROPERTIES} ${properties} PARENT_SCOPE)
  elseif()
    if(NOT TEST ${TEST_NAME})
      message(FATAL_ERROR "\'${TEST_NAME}\' Is not a test")
    endif()
    if (ARGS_FOR_GTEST_DISCOVER_TESTS)
      set_tests_properties(
        ${TEST_NAME} PROPERTIES ${properties}
      )
    else()
      set_tests_properties(
        ${TEST_NAME} PROPERTIES ENVIRONMENT "${properties}"
      )
    endif()
  endif()
  # There is a bug which prevents environment variables from
  # being available during test discovery on windows
  # https://gitlab.kitware.com/cmake/cmake/-/issues/21453
  if(WIN32 AND NOT MSVC)
    configure_env_injection(
      DIRECTORY ${ARGS_DIRECTORY}
      VARIABLES ${ARGS_VARIABLES}
    )
  endif()
endfunction()


function(set_tests_runtime_paths TEST_NAME)
  include(BuildTools)
  set(options PREPEND ADD_CMAKE_PREFIX_PATHS)
  set(oneValueArgs PATH_VARIABLE)
  set(multiValueArgs PATHS ADDITIONAL_ENV_VARIABLES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_ADD_CMAKE_PREFIX_PATHS)
    get_runtime_directory_suffix(runtime_suffix)
    foreach(d ${CMAKE_PREFIX_PATH})
      cmake_path(APPEND d ${runtime_suffix})
      list(APPEND ARGS_PATHS ${d})
    endforeach()
  endif()
  if(ARGS_PATHS)
    if(NOT ARGS_PATH_VARIABLE)
      get_runtime_environment_var(ARGS_PATH_VARIABLE)
    endif()
    if(ARGS_PREPEND)
      set(PREPEND_TOKEN PREPEND)
    endif()
    update_env_path(
      PATH_VARIABLE ${ARGS_PATH_VARIABLE}
      ${PREPEND_TOKEN}
      OUTPUT_VARIABLE UPDATED_PATHS
      PATHS ${ARGS_PATHS}
    )
    list(APPEND ARGS_ADDITIONAL_ENV_VARIABLES "${ARGS_PATH_VARIABLE}=${UPDATED_PATHS}")
  endif()
  if(ARGS_ADDITIONAL_ENV_VARIABLES)
    set_tests_environment(
      ${TEST_NAME}
      ${ARGS_UNPARSED_ARGUMENTS}
      VARIABLES ${ARGS_ADDITIONAL_ENV_VARIABLES}
    )
  endif()
endfunction()
