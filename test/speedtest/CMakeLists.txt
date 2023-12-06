cmake_minimum_required(VERSION 3.16)
project(SpeedTestCXX)

option(LANGUAGE "Language of speedtest to build" CXX)
option(N_MSG "Number of messages that should be sent" 100)
option(S_MSG "Size of messages that should be sent" 100)
option(COMM "Communicator type that should be used" DEFAULT_COMM)
set(SPEED_TEST_N_MSG ${N_MSG})
set(SPEED_TEST_S_MSG ${S_MSG})
set(SPEED_TEST_COMM ${COMM})

enable_testing()
add_subdirectory(${LANGUAGE})
