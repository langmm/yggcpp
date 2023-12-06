cmake_minimum_required(VERSION 3.16)
project(SpeedTestCXX)

find_package(YggInterface REQUIRED)

configure_file(speedtest.cpp.in speedtest.cpp @ONLY)
add_executable(speedtest_cpp ${CMAKE_CURRENT_BINARY_DIR}/speedtest.cpp)
target_link_libraries(speedtest_cpp PRIVATE YggInterface::CXX)

add_test(NAME speedtest_cpp COMMAND speedtest_cpp)
