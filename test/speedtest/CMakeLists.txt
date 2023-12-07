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

function(add_speed_test LANGUAGE LANGUAGE_EXT)
  if (NOT ENABLE_${LANGUAGE})
    return()
  endif()
  configure_file(CMakeLists.txt.in ${LANGUAGE}/CMakeLists.txt @ONLY)
  configure_file(${LANGUAGE}/speedtest.${LANGUAGE_EXT}.in
                 ${LANGUAGE}/speedtest.${LANGUAGE_EXT} @ONLY)
  set(lang_src_dir ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE})
  set(lang_bld_dir ${CMAKE_CURRENT_BINARY_DIR}/${LANGUAGE}_build)
  file(MAKE_DIRECTORY ${lang_bld_dir})
  execute_process(
    COMMAND ${CMAKE_COMMAND} ${lang_src_dir} -DYggInterface_DIR=${YggInterface_DIR} -DCMAKE_VERBOSE_MAKEFILE=ON
    WORKING_DIRECTORY ${lang_bld_dir}
    COMMAND_ECHO STDOUT
    RESULT_VARIABLE ret)
  if (NOT ret EQUAL 0)
    message(FATAL_ERROR "Failed to configure ${LANGUAGE} speedtest")
  endif()
  execute_process(
    COMMAND ${CMAKE_COMMAND} --build .
    WORKING_DIRECTORY ${lang_bld_dir}
    COMMAND_ECHO STDOUT
    RESULT_VARIABLE ret)
  if (NOT ret EQUAL 0)
    message(FATAL_ERROR "Failed to build ${LANGUAGE} speedtest")
  endif()
  add_test(NAME speedtest_${LANGUAGE}
           COMMAND ${LANGUAGE}_build/speedtest_${LANGUAGE})
endfunction()

enable_testing()
add_speed_test(C c)
add_speed_test(CXX cpp)
add_speed_test(Fortran f90)
if (ENABLE_Python)
  configure_file(Python/speedtest.py.in Python/speedtest.py @ONLY)
  add_test(NAME speedtest_Python COMMAND python Python/speedtest.py)
endif()
