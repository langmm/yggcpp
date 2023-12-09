include(CTest)
include(SearchTools)
find_package_python()
enable_testing()
add_test(NAME python_tests COMMAND ${Python3_EXECUTABLE} -m pytest -sv
         ${CMAKE_CURRENT_SOURCE_DIR})
