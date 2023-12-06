cmake_minimum_required(VERSION 3.16)
project(SpeedTestFortran Fortran)

find_package(YggInterface REQUIRED)

configure_file(speedtest.f90.in speedtest.f90 @ONLY)
add_executable(speedtest_for ${CMAKE_CURRENT_BINARY_DIR}/speedtest.f90)
target_link_libraries(speedtest_for PRIVATE YggInterface::Fortran)

add_test(NAME speedtest_for COMMAND speedtest_for)
