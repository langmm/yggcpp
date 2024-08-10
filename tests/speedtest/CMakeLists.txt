cmake_minimum_required(VERSION 3.16)
project(SpeedTest)
set(CMAKE_VERBOSE_MAKEFILE ON)

option(ENABLE_CXX "Turn on CXX speed test" ON)
option(ENABLE_C "Turn on C speed test" ON)
option(ENABLE_Fortran "Turn on Fortran speed test" ON)
option(ENABLE_Python "Turn on Python speed test" ON)
option(N_MSG "Number of messages that should be sent" 100)
option(S_MSG "Size of messages that should be sent" 100)
option(COMM "Communicator type that should be used" DEFAULT_COMM)
set(SPEED_TEST_N_MSG ${N_MSG})
set(SPEED_TEST_S_MSG ${S_MSG})
set(SPEED_TEST_COMM ${COMM})
list(APPEND CMAKE_COMMAND_ARGS_TO_COPY YggInterface_DIR CMAKE_PREFIX_PATH ${Python_PREFIX}_ROOT_DIR CMAKE_VERBOSE_MAKEFILE)

function(add_speed_test LANGUAGE LANGUAGE_EXT)
  if (NOT ENABLE_${LANGUAGE})
    return()
  endif()
  find_package(YggInterface REQUIRED)
  include(YggAddFortranSubdirectory)
  include(TestTools)
  include(BuildTools)
  check_language_external(${LANGUAGE} REQUIRED OUTPUT_VARIABLE external
                          GENERATOR generator EXTERNAL_PATHS paths)
  if (external)
    list(APPEND CMAKE_COMMAND_ARGS -DCMAKE_${LANGUAGE}_COMPILER:PATH=${CMAKE_${LANGUAGE}_COMPILER})
    if (generator)
      list(APPEND CMAKE_COMMAND_ARGS "-G${generator}")
    endif()
  endif()
  cmakevars2cmakecliargs(CMAKE_COMMAND_ARGS ${CMAKE_COMMAND_ARGS_TO_COPY})
  configure_file(speedtest.${LANGUAGE_EXT}.in
                 ${LANGUAGE}/speedtest.${LANGUAGE_EXT} @ONLY)
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
    # OUTPUT_PROPERTIES NEW_TEST_ENV
    ADD_CMAKE_PREFIX_PATHS
  )
  message(STATUS "NEW_TEST_ENV = ${NEW_TEST_ENV}")
  # list(JOIN NEW_TEST_ENV "\;" NEW_TEST_ENV)
  # message(STATUS "NEW_TEST_ENV = ${NEW_TEST_ENV}")
  # set_tests_properties(speedtest_${LANGUAGE} PROPERTIES
  #                      ENVIRONMENT "${NEW_TEST_ENV}")
endfunction()

enable_testing()
add_speed_test(C c)
add_speed_test(CXX cpp)
add_speed_test(Fortran f90)
if (ENABLE_Python)
  find_package(YggInterface)
  if (NOT YggInterface_FOUND)
    set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/../../cmake")
  endif()
  include(SearchTools)
  find_package_python()
  configure_file(speedtest.py.in Python/speedtest.py @ONLY)
  add_test(NAME speedtest_Python COMMAND ${${Python_PREFIX}_EXECUTABLE} Python/speedtest.py)
endif()
