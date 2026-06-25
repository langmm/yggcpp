cmake_minimum_required(VERSION 3.16)
project(SpeedTest)

option(ENABLE_CXX "Turn on CXX speed test" ON)
option(ENABLE_C "Turn on C speed test" ON)
option(ENABLE_Fortran "Turn on Fortran speed test" ON)
option(ENABLE_Python "Turn on Python speed test" ON)

option(ENABLE_ZMQ "Turn on ZeroMQ speed test" ON)
option(ENABLE_IPC "Turn on IPC speed test" ON)
option(ENABLE_RMQ "Turn on RabbitMQ speed test" ON)
option(ENABLE_MPI "Turn on MPI API speed test" OFF)
option(ENABLE_REST "Turn on REST API speed test" OFF)

set(N_MSG 100 CACHE STRING "Number of messages that should be sent")
set(S_MSG 100 CACHE STRING "Size of messages that should be sent")
set(COMM "DEFAULT" CACHE STRING "Communicator type that should be used")
set(SPEED_TEST_N_MSG ${N_MSG})
set(SPEED_TEST_S_MSG ${S_MSG})
set(SPEED_TEST_COMM "${COMM}_COMM")
set(SPEED_TEST_DIR ${CMAKE_CURRENT_BINARY_DIR})
option(YGG_INSPECT_TARGETS "Inspect the executables and libraries that are built" OFF)
list(APPEND CMAKE_COMMAND_ARGS_TO_COPY YggInterface_DIR
     CMAKE_GENERATOR CMAKE_PREFIX_PATH
     CMAKE_VERBOSE_MAKEFILE CMAKE_MESSAGE_LOG_LEVEL
     CMAKE_BUILD_PARALLEL_LEVEL YGG_INSPECT_TARGETS)

function(add_speed_test LANGUAGE LANGUAGE_EXT)
  set(options)
  set(oneValueArgs COMM)
  set(multiValueArgs)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ENABLE_${LANGUAGE})
    return()
  endif()
  if(LANGUAGE STREQUAL "Python")
    # CMake package configuration files are not installed for only Python
    find_package(YggInterface)
  else()
    find_package(YggInterface REQUIRED)
  endif()
  if(NOT YggInterface_FOUND)
    set(YGG_COMMS_AVAILABLE ZMQ RMQ REST)
    if(UNIX)
      list(APPEND YGG_COMMS_AVAILABLE IPC)
    endif()
  endif()
  if(ARGS_COMM)
    set(COMM "${ARGS_COMM}")
    set(SPEED_TEST_COMM "${ARGS_COMM}_COMM")
  endif()
  if(COMM STREQUAL "ALL")
    foreach(icomm IN LISTS YGG_COMMS_AVAILABLE)
      add_speed_test(
        ${LANGUAGE} ${LANGUAGE_EXT}
        COMM ${icomm}
      )
    endforeach()
    return()
  elseif(NOT (COMM STREQUAL "DEFAULT" OR ENABLE_${COMM}))
    message(WARNING "${COMM} communicator test not enabled")
    return()
  elseif(COMM STREQUAL "MPI" OR COMM STREQUAL "REST")
    message(WARNING "${COMM} communicators requires additional setup beyond the scope of the simple speedtest as currently implemented")
    return()
  endif()
  if(YggInterface_FOUND)
    include(TestTools)
    include(BuildTools)
    check_yggdrasil_option(${COMM} TYPE COMM REQUIRED)
  endif()
  set(SOURCE_FILE "${LANGUAGE}_${COMM}/speedtest.${LANGUAGE_EXT}")
  configure_file(speedtest.${LANGUAGE_EXT}.in
                 ${SOURCE_FILE} @ONLY)
  message(STATUS "ADD_SPEED_TEST: ${LANGUAGE} ${COMM}")
  if (LANGUAGE STREQUAL "Python")
    if(YggInterface_FOUND)
      find_yggdrasil_dependency(${LANGUAGE} REQUIRED)
    else()
      find_package(Python3 COMPONENTS Interpreter REQUIRED)
      if(NOT ${LANGUAGE}_EXECUTABLE)
        set(${LANGUAGE}_EXECUTABLE ${Python3_EXECUTABLE})
      endif()
    endif()
    message(STATUS "${LANGUAGE}_EXECUTABLE = ${${LANGUAGE}_EXECUTABLE}")
    if(NOT ${LANGUAGE}_EXECUTABLE)
      message(FATAL_ERROR "${LANGUAGE}_EXECUTABLE not set")
    endif()
    add_test(
      NAME speedtest_${LANGUAGE}_${COMM}
      COMMAND ${${LANGUAGE}_EXECUTABLE} ${SOURCE_FILE}
    )
    return()
  endif()
  check_yggdrasil_option(${LANGUAGE} TYPE LANGUAGE REQUIRED)
  if(WIN32 AND LANGUAGE STREQUAL "Fortran")
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
    list(REMOVE_ITEM CMAKE_COMMAND_ARGS_TO_COPY CMAKE_GENERATOR)
    list(APPEND CMAKE_COMMAND_ARGS
         "-DCMAKE_${LANGUAGE}_COMPILER:PATH=${EXTERNAL_COMPILER}"
         "-DCMAKE_${LANGUAGE}_COMPILER_LINKER:PATH=${EXTERNAL_LINKER}"
         "-DCMAKE_CXX_COMPILER_LINKER:PATH=${EXTERNAL_LINKER}"
         "-DCMAKE_LINKER:PATH=${EXTERNAL_LINKER}"
         -G "${EXTERNAL_GENERATOR}")
    message(DEBUG "CMAKE_PREFIX_PATH = ${CMAKE_PREFIX_PATH}")
    if(WIN32 AND EXTERNAL_LINKER_MSVC AND NOT EXTERNAL_COMPILER_MSVC)
      list(
        APPEND CMAKE_COMMAND_ARGS
        "-DCMAKE_LINKER_LINK=${EXTERNAL_LINKER}"
      )
    endif()
    message(DEBUG "EXTERNAL_COMPILER = ${EXTERNAL_COMPILER}")
    message(DEBUG "EXTERNAL_LINKER = ${EXTERNAL_LINKER}")
    message(DEBUG "EXTERNAL_GENERATOR = ${EXTERNAL_GENERATOR}")
  endif()
  message(DEBUG "CMAKE_COMMAND_ARGS_TO_COPY = ${CMAKE_COMMAND_ARGS_TO_COPY}")
  cmakevars2cmakecliargs(CMAKE_COMMAND_ARGS ${CMAKE_COMMAND_ARGS_TO_COPY})
  message(DEBUG "CMAKE_COMMAND_ARGS = ${CMAKE_COMMAND_ARGS}")
  configure_and_build(
    CMakeLists.txt.in
    SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE}_${COMM}
    BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE}_${COMM}_build
    PREPEND_PATHS ${paths}
    CMAKE_COMMAND_ARGS ${CMAKE_COMMAND_ARGS}
  )
  add_test(NAME speedtest_${LANGUAGE}_${COMM}
           COMMAND ${LANGUAGE}_${COMM}_build/speedtest_${LANGUAGE})
  if(WIN32 AND Python3_RUNTIME_LIBRARY_DIRS)
    list(APPEND TEST_ENV "PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
  else()
    list(APPEND TEST_ENV "FAKE_ENV=test")
  endif()
  set_tests_runtime_paths(
    speedtest_${LANGUAGE}_${COMM}
    PATHS ${NEW_PATHS}
    ADDITIONAL_ENV_VARIABLES ${TEST_ENV}
    ADD_CMAKE_PREFIX_PATHS
  )
endfunction()

enable_testing()
add_speed_test(C c)
add_speed_test(CXX cpp)
add_speed_test(Fortran f90)
add_speed_test(Python py)
