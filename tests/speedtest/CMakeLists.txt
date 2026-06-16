cmake_minimum_required(VERSION 3.16)
project(SpeedTest)

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
option(YGG_INSPECT_TARGETS "Inspect the executables and libraries that are built" OFF)
list(APPEND CMAKE_COMMAND_ARGS_TO_COPY YggInterface_DIR CMAKE_PREFIX_PATH
     CMAKE_VERBOSE_MAKEFILE CMAKE_MESSAGE_LOG_LEVEL
     CMAKE_BUILD_PARALLEL_LEVEL YGG_INSPECT_TARGETS)

function(add_speed_test LANGUAGE LANGUAGE_EXT)
  if (NOT ENABLE_${LANGUAGE})
    return()
  endif()
  find_package(YggInterface REQUIRED)
  include(TestTools)
  include(BuildTools)
  check_yggdrasil_option(${COMM} TYPE COMM REQUIRED)
  set(OUTPUT_FILE "${LANGUAGE}/speedtest.${LANGUAGE_EXT}")
  configure_file(speedtest.${LANGUAGE_EXT}.in
                 ${OUTPUT_FILE} @ONLY)
  message(STATUS "ADD_SPEED_TEST: ${LANGUAGE}")
  if (LANGUAGE STREQUAL "Python")
    find_yggdrasil_dependency(${LANGUAGE} REQUIRED)
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
  check_yggdrasil_option(${LANGUAGE} TYPE LANGUAGE REQUIRED)
  find_compiler_external(
    ${LANGUAGE} REQUIRED
    OUTPUT_COMPILER EXTERNAL_COMPILER
    OUTPUT_GENERATOR EXTERNAL_GENERATOR
    OUTPUT_MSVC EXTERNAL_COMPILER_MSVC
  )
  find_compiler_external(
    CXX REQUIRED
    OUTPUT_LINKER EXTERNAL_LINKER
    OUTPUT_MSVC EXTERNAL_LINKER_MSVC
  )
  get_native_directory("${EXTERNAL_COMPILER}" paths)
  list(APPEND CMAKE_COMMAND_ARGS
       "-DCMAKE_${LANGUAGE}_COMPILER:PATH=${EXTERNAL_COMPILER}"
       "-DCMAKE_${LANGUAGE}_COMPILER_LINKER:PATH=${EXTERNAL_LINKER}"
       "-DCMAKE_CXX_COMPILER_LINKER:PATH=${EXTERNAL_LINKER}"
       "-DCMAKE_LINKER:PATH=${EXTERNAL_LINKER}"
       -G "${EXTERNAL_GENERATOR}")
  message(DEBUG "CMAKE_PREFIX_PATH = ${CMAKE_PREFIX_PATH}")
  message(DEBUG "CMAKE_COMMAND_ARGS_TO_COPY = ${CMAKE_COMMAND_ARGS_TO_COPY}")
  cmakevars2cmakecliargs(CMAKE_COMMAND_ARGS ${CMAKE_COMMAND_ARGS_TO_COPY})
  if(WIN32 AND EXTERNAL_LINKER_MSVC AND NOT EXTERNAL_COMPILER_MSVC)
    list(
      APPEND CMAKE_COMMAND_ARGS
      "-DCMAKE_${LANGUAGE}_LINK_MODE=LINKER"
      "-DCMAKE_${LANGUAGE}_USING_LINKER_MSVC=${EXTERNAL_LINKER}"
      "-DCMAKE_CXX_LINK_MODE=LINKER"
      "-DCMAKE_LINKER_TYPE=MSVC"
      "-DCMAKE_LINKER_LINK=${EXTERNAL_LINKER}"
      "-DCMAKE_LINKER_MSVC=${EXTERNAL_LINKER}"
      "-DCMAKE_CXX_USING_LINKER_MSVC=${EXTERNAL_LINKER}"
    )
  endif()
  message(DEBUG "CMAKE_COMMAND_ARGS = ${CMAKE_COMMAND_ARGS}")
  message(DEBUG "EXTERNAL_COMPILER = ${EXTERNAL_COMPILER}")
  message(DEBUG "EXTERNAL_LINKER = ${EXTERNAL_LINKER}")
  message(DEBUG "EXTERNAL_GENERATOR = ${EXTERNAL_GENERATOR}")
  configure_and_build(
    CMakeLists.txt.in
    SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE}
    BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE}_build
    PREPEND_PATHS ${paths}
    CMAKE_COMMAND_ARGS ${CMAKE_COMMAND_ARGS}
  )
  add_test(NAME speedtest_${LANGUAGE}
           COMMAND ${LANGUAGE}_build/speedtest_${LANGUAGE})
  if(WIN32 AND Python3_RUNTIME_LIBRARY_DIRS)
    list(APPEND TEST_ENV "PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
  else()
    list(APPEND TEST_ENV "FAKE_ENV=test")
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
  find_package(YggInterface REQUIRED)
  find_yggdrasil_dependency(${LANGUAGE} REQUIRED)
  set(OUTPUT_FILE "${LANGUAGE}/speedtest.${LANGUAGE_EXT}")
  configure_file(speedtest.${LANGUAGE_EXT}.in
                 ${OUTPUT_FILE} @ONLY)
  if(LANGUAGE STREQUAL "Python")
    set(Python_EXECUTABLE ${Python3_EXECUTABLE})
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
