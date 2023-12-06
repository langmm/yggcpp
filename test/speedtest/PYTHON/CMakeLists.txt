cmake_minimum_required(VERSION 3.16)
project(SpeedTestPython)

find_package(YggInterface REQUIRED)

configure_file(speedtest.py.in speedtest.py @ONLY)

enable_testing()
add_test(NAME speedtest_py COMMAND python ${CMAKE_CURRENT_BINARY_DIR}/speedtest.py)
