enable_testing()

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")
find_package(GTestSrc)

find_package(cmocka)
if(cmocka_FOUND)
    message(STATUS "Found CMOCKA")
else()
    include(FetchContent)
    FetchContent_Declare(
            cmocka_ygg
            GIT_REPOSITORY https://gitlab.com/cmocka/cmocka.git
            GIT_TAG f5e2cd77c88d9f792562888d2b70c5a396bfbf7a
    )
    FetchContent_MakeAvailable(cmocka_ygg)
endif()

if(GTESTSRC_FOUND)

    if (WIN32 AND (NOT CYGWIN) AND (NOT MINGW))
        set(gtest_disable_pthreads ON)
        set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    endif()

    IF(GTEST_SOURCE_DIR)
        add_subdirectory(${GTEST_SOURCE_DIR} ${CMAKE_BINARY_DIR}/googletest)
    endif()
    include_directories(SYSTEM ${GTEST_INCLUDE_DIR})

    set(TEST_LIBRARIES gtest gtest_main)
endif()

set(TEST_LIBRARIES ${TEST_LIBRARIES} YggInterface ${Boost_LOG_LIBRARY} ${Boost_REGEX_LIBRARY} dl)
set(TEST_INCLUDE_DIRS ${Boost_INCLUDE_DIRS} ${RAPIDJSON_INCLUDE_DIRS})
if (CONDA_PREFIX)
    set(TEST_INCLUDE_DIRS ${TEST_INCLUDE_DIRS} ${CONDA_PREFIX}/include)
endif()
if (NOT YGGDRASIL_DISABLE_PYTHON_C_API)
    set(TEST_LIBRARIES ${TEST_LIBRARIES} ${Python3_LIBRARIES} Python3::NumPy ${Python3_NumPy_LIBRARIES})
    set(TEST_INCLUDE_DIRS ${TEST_INCLUDE_DIRS} ${Python3_INCLUDE_DIRS} ${Python3_NumPy_INCLUDE_DIRS})
endif()
if (ZeroMQ_FOUND)
    set(TEST_LIBRARIES ${TEST_LIBRARIES} ${ZeroMQ_LIBRARY})
    set(TEST_INCLUDE_DIRS ${TEST_INCLUDE_DIRS} ${ZeroMQ_INCLUDE_DIR})
endif()
if (OpenMP_CXX_FOUND)
    set(TEST_LIBRARIES ${TEST_LIBRARIES} OpenMP::OpenMP_CXX)
endif()

include_directories(SYSTEM ${TEST_INCLUDE_DIRS})

if(GTESTSRC_FOUND)
    option(YGG_ENABLE_COVERAGE "Enable coverage reporting" OFF)
    if(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
        message("COVERAGE ON")
        set(CMAKE_CXX_FLAGS "-g -O0 --coverage -fprofile-arcs -ftest-coverage -fprofile-filter-files=\"${CMAKE_SOURCE_DIR}/communication/*\" -fpermissive")
        set(CMAKE_C_FLAGS "-g -O0 --coverage -fprofile-arcs -ftest-coverage -fprofile-filter-files=\"${CMAKE_SOURCE_DIR}/communication/*\" -fpermissive")
        if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
            set(CTEST_COVERAGE_COMMAND "llvm-cov")
            set(CTEST_COVERAGE_EXTRA_FLAGS "gcov")
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
            set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
        else()
            set(CTEST_COVERAGE_COMMAND "gcov")
            set(CTEST_COVERAGE_EXTRA_FLAGS "")
        endif()
    endif(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")


    include(unittest/CMakeLists.txt)
    #add_custom_target(tests ALL)
    #add_subdirectory(perftest)
    #add_subdirectory(unittest)
    add_definitions(-DYGG_TEST)

endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
   add_definitions(-DELF_AVAILABLE)
endif()
