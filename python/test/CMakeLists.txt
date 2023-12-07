include(CTest)
enable_testing()
add_test(NAME python_tests COMMAND python3 -m pytest -sv
         ${CMAKE_CURRENT_SOURCE_DIR})
