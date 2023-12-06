cmake_minimum_required(VERSION 3.16)
project(SpeedTestC)

find_package(YggInterface REQUIRED)

configure_file(speedtest.c.in speedtest.c @ONLY)
add_executable(speedtest_c ${CMAKE_CURRENT_BINARY_DIR}/speedtest.c)
target_link_libraries(speedtest_c PRIVATE YggInterface::C)

add_test(NAME speedtest_c COMMAND speedtest_c)
