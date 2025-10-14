cmake_minimum_required(VERSION 3.16)
project(SpeedTest)
set(CMAKE_VERBOSE_MAKEFILE ON)

option(ENABLE_CXX "Turn on CXX speed test" ON)
option(ENABLE_C "Turn on C speed test" ON)
option(ENABLE_Fortran "Turn on Fortran speed test" ON)
option(ENABLE_Python "Turn on Python speed test" ON)
set(N_MSG 100 CACHE STRING "Number of messages that should be sent")
set(S_MSG 100 CACHE STRING "Size of messages that should be sent")
set(COMM "DEFAULT" CACHE STRING "Communicator type that should be used")
set(SPEED_TEST_N_MSG ${N_MSG})
set(SPEED_TEST_S_MSG ${S_MSG})
set(SPEED_TEST_COMM "${COMM}_COMM")
set(SPEED_TEST_DIR ${CMAKE_CURRENT_BINARY_DIR})
list(APPEND CMAKE_COMMAND_ARGS_TO_COPY YggInterface_DIR CMAKE_PREFIX_PATH ${Python_PREFIX}_ROOT_DIR CMAKE_VERBOSE_MAKEFILE)

function(add_speed_test LANGUAGE LANGUAGE_EXT)
  if (NOT ENABLE_${LANGUAGE})
    return()
  endif()
  find_package(YggInterface REQUIRED)
  include(TestTools)
  include(BuildTools)
  include(AddYggInterface)
  check_yggdrasil_option(${LANGUAGE} TYPE LANGUAGE REQUIRED)
  check_yggdrasil_option(${COMM} TYPE COMM REQUIRED)
  set(OUTPUT_FILE "${LANGUAGE}/speedtest.${LANGUAGE_EXT}")
  configure_file(speedtest.${LANGUAGE_EXT}.in
                 ${OUTPUT_FILE} @ONLY)
  if (LANGUAGE STREQUAL "Python")
    # find_yggdrasil_dependency(${LANGUAGE} REQUIRED)
    if(LANGUAGE STREQUAL "Python")
      set(Python_EXECUTABLE ${${Python_PREFIX}_EXECUTABLE})
    endif()
    message(STATUS "${LANGUAGE}_EXECUTABLE = ${${LANGUAGE}_EXECUTABLE}")
    if(NOT ${LANGUAGE}_EXECUTABLE)
      message(FATAL_ERROR "${LANGUAGE}_EXECUTABLE not set")
    endif()
    add_test(
      NAME speedtest_${LANGUAGE}
      COMMAND ${${LANGUAGE}_EXECUTABLE} ${OUTPUT_FILE}
    )
    return()
  endif()
  check_language_external(${LANGUAGE} REQUIRED OUTPUT_VARIABLE external
                          GENERATOR generator EXTERNAL_PATHS paths)
  if (external)
    list(APPEND CMAKE_COMMAND_ARGS -DCMAKE_${LANGUAGE}_COMPILER:PATH=${CMAKE_${LANGUAGE}_COMPILER})
    if (generator)
      list(APPEND CMAKE_COMMAND_ARGS "-G${generator}")
    endif()
  endif()
  cmakevars2cmakecliargs(CMAKE_COMMAND_ARGS ${CMAKE_COMMAND_ARGS_TO_COPY})
  configure_and_build(
    CMakeLists.txt.in
    SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE}
    BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE}_build
    PREPEND_PATHS ${paths}
    CMAKE_COMMAND_ARGS ${CMAKE_COMMAND_ARGS}
  )
  add_test(NAME speedtest_${LANGUAGE}
           COMMAND ${LANGUAGE}_build/speedtest_${LANGUAGE})
  if(WIN32 AND ${Python_PREFIX}_RUNTIME_LIBRARY_DIRS)
    list(APPEND TEST_ENV "PYTHONHOME=${${Python_PREFIX}_RUNTIME_LIBRARY_DIRS}")
  else()
    list(APPEND TEST_ENV "FAKE_ENV=test")
  endif()
  if (WIN32 AND ${Python_PREFIX}_ROOT_DIR)
    list(APPEND NEW_PATHS ${${Python_PREFIX}_ROOT_DIR})
  endif()
  set_tests_runtime_paths(
    speedtest_${LANGUAGE}
    PATHS ${NEW_PATHS}
    ADDITIONAL_ENV_VARIABLES ${TEST_ENV}
    ADD_CMAKE_PREFIX_PATHS
  )
endfunction()

function(add_speed_test_interp LANGUAGE LANGUAGE_EXT)
  if (NOT ENABLE_${LANGUAGE})
    return()
  endif()
  find_package(YggInterface)
  if (NOT YggInterface_FOUND)
    set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/../../cmake")
  endif()
  include(AddYggInterface)
  find_yggdrasil_dependency(${LANGUAGE} REQUIRED)
  set(OUTPUT_FILE "${LANGUAGE}/speedtest.${LANGUAGE_EXT}")
  configure_file(speedtest.${LANGUAGE_EXT}.in
                 ${OUTPUT_FILE} @ONLY)
  if(LANGUAGE STREQUAL "Python")
    set(Python_EXECUTABLE ${${Python_PREFIX}_EXECUTABLE})
  endif()
  add_test(
    NAME speedtest_${LANGUAGE}
    COMMAND ${${LANGUAGE}_EXECUTABLE} ${OUTPUT_FILE}
  )
endfunction()

enable_testing()
add_speed_test(C c)
add_speed_test(CXX cpp)
add_speed_test(Fortran f90)
add_speed_test(Python py)
