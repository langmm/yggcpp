cmake_minimum_required(VERSION 3.16)
set(CMAKE_VERBOSE_MAKEFILE True)
if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)

if (POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif()


project(YggInterfaceCPP VERSION 0.1.0.0)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS)
include(FetchContent)
FetchContent_Declare(
        RapidJSON_ygg
        GIT_REPOSITORY https://github.com/astro-friedel/rapidjson_ygg.git
        GIT_TAG 11179034d8fce7216d01f5183102ce9bfb044401
)

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
    message(FATAL_ERROR "In-source build prohibited.")
endif("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")
set(CMAKE_INSTALL_PREFIX /home/friedel/devel/yggtests)

#file(GLOB_RECURSE SOURCES "communication/**/*.cpp")
file(GLOB SOURCES "communication/utils/*.cpp" "communication/communicators/*.cpp")
add_library(YggInterface SHARED ${SOURCES})

option(YGG_BUILD_TESTS "Build tests" ON)
option(YGG_BUILD_THIRDPARTY_GTEST "Use gtest installation in 'thirdpart/gtest' by default if available" ON)

set(YGG_BUILD_FLAGS "")
option(ENABLE_ZMQ "Enable zeromq" ON)
#find cppzmq wrapper, installed by make of cppzmq
########################################################################
# LIBZMQ dependency
########################################################################
if(ENABLE_ZMQ)
    include(ExternalProject)

    EXTERNALPROJECT_ADD(
            yggLibzmq
            URL https://github.com/zeromq/libzmq/archive/refs/tags/v4.3.4.tar.gz
            CMAKE_ARGS -DCMAKE_PREFIX_PATH=${CMAKE_INSTALL_PREFIX} -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} -DBUILD_TESTS=0 -DENABLE_DRAFTS=1 -DWITH_DOCS=0 -DZMQ_BUILD_TESTS=0 -DZMQ_OUTPUT_BASENAME=yggzmq
    )
    EXTERNALPROJECT_GET_PROPERTY(yggLibzmq install_dir)
    set(LIB_ZMQ_INSTALL_DIR install_dir)

    EXTERNALPROJECT_ADD(
            yggcppzmq
            DEPENDS yggLibzmq
            URL https://github.com/zeromq/cppzmq/archive/refs/tags/v4.9.0.zip
            CMAKE_ARGS -DCMAKE_PREFIX_PATH=${LIB_ZMQ_INSTALL_DIR} -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} -DCPPZMQ_BUILD_TESTS=0 -DCATCH_INSTALL_DOCS=0 -DCATCH_BUILD_TESTING=0 -DENABLE_DRAFTS=1
    )

    add_dependencies(YggInterface yggLibzmq yggcppzmq)
    add_definitions(-DZMQINSTALLED -DZMQ_BUILD_DRAFT_API)

    include_directories(${CMAKE_INSTALL_PREFIX}/include/libzmq
            ${CMAKE_INSTALL_PREFIX}/include/cppzmq
            ${CMAKE_INSTALL_PREFIX}/include)
    add_library(zmqlib SHARED IMPORTED)
    set_target_properties(zmqlib PROPERTIES IMPORTED_LOCATION ${CMAKE_INSTALL_PREFIX}/lib/libyggzmq.so)
    target_link_libraries(YggInterface zmqlib)
else (ENABLE_ZMQ)
    message("No zmq support will be built." )
endif (ENABLE_ZMQ)

find_package(Boost REQUIRED COMPONENTS log)
include_directories(${Boost_INCLUDE_DIRS})
add_definitions(-DBOOST_LOG_DYN_LINK)
#if(Boost_date_time_FOUND)
#    message("DATE TIME")
#    add_definitions(-D_YGGIPC)
#endif()
add_definitions(-D_YGGIPC)


find_package(OpenMP)
if (OpenMP_CXX_FOUND)
    add_definitions(-D_OPENMP -DMPIINSTALLED)
    message("MPI found")
    target_link_libraries(YggInterface OpenMP::OpenMP_CXX)
else()
    message("NO OpenMP")
endif()

find_package(MPI)
if(MPI_CXX_FOUND)
    target_link_libraries(YggInterface MPI::MPI_CXX)
ENDIF()

#get_cmake_property(_variableNames VARIABLES)
#list (SORT _variableNames)
#foreach (_variableName ${_variableNames})
#    message(STATUS "${_variableName}=${${_variableName}}")
#endforeach()


find_package(Python3 COMPONENTS Interpreter Development NumPy REQUIRED)
include_directories(${Python_INCLUDE_DIRS})
include_directories(${Python3_NumPy_INCLUDE_DIRS})
set(YGG_PYTHON_ENV "DATADIR=${CMAKE_SOURCE_DIR}/test;YGG_PYTHON_EXEC=${Python3_EXECUTABLE}")

# There is an issue with embedding Python on Windows in conda environments
# https://github.com/ContinuumIO/anaconda-issues/issues/11374
if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(YGG_PYTHON_ENV "${YGG_PYTHON_ENV};PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
endif()

target_link_libraries(YggInterface ${PYTHON_LIBRARIES} Python3::NumPy)
target_link_libraries(YggInterface ${Boost_LOG_LIBRARY})
if (Boost_DATE_TIME_FOUND)
    target_link_libraries(YggInterface ${Boost_DATE_TIME_LIBRARY})
else()
    add_definitions(-DBOOST_DATE_TIME_NO_LIB)
endif()

include_directories("communication")

FetchContent_MakeAvailable(RapidJSON_ygg)
include_directories("${rapidjson_ygg_SOURCE_DIR}/include")
message("XYZ  ${rapidjson_ygg_SOURCE_DIR}/include")
add_definitions(-DRAPIDJSON_YGGDRASIL)
add_definitions(-DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION)
add_definitions(-D_USE_MATH_DEFINES)

IF (WIN32)
    ADD_LIBRARY(regex_win32 STATIC IMPORTED)
    SET_TARGET_PROPERTIES(
            regex_win32 PROPERTIES
            IMPORTED_LOCATION C:\\Users\\Meagan\\Code\\yggdrasil\\yggdrasil\\regex\\build\\Debug\\regex_win32.lib)
    TARGET_LINK_LIBRARIES(YggInterface regex_win32)
    if (libzmq_FOUND)
        ADD_LIBRARY(zmq STATIC IMPORTED)
        SET_TARGET_PROPERTIES(
                zmq PROPERTIES
                IMPORTED_LOCATION C:\\projects\\build_libzmq\\lib\\Debug\\zmq.lib)
        TARGET_LINK_LIBRARIES(YggInterface zmq)
    endif()
    if (libczmq_FOUND)
        ADD_LIBRARY(czmq STATIC IMPORTED)
        SET_TARGET_PROPERTIES(
                czmq PROPERTIES
                IMPORTED_LOCATION C:\\projects\\build_czmq\\Debug\\czmq.lib)
        TARGET_LINK_LIBRARIES(YggInterface czmq)
    endif()
ELSE(WIN32)
    if (libzmq_FOUND)
        target_link_libraries(YggInterface ${libzmq_LIBRARIES})
    endif()
ENDIF(WIN32)

if(YGG_BUILD_TESTS)
    if(MSVC11)
        # required for VS2012 due to missing support for variadic templates
        add_definitions(-D_VARIADIC_MAX=10)
    endif(MSVC11)

    add_subdirectory(test)
    include(CTest)
    add_definitions(-DYGG_TEST)
    if(YGG_ENABLE_COVERAGE)
        target_link_libraries(YggInterface gcov)
        # find required tools
        find_program(LCOV lcov REQUIRED)
        find_program(GENHTML genhtml REQUIRED)
        add_custom_target(coverage
                # gather data
                COMMAND ${LCOV} --rc lcov_branch_coverage=1 --exclude '/usr/*' --exclude '/opt/*' --directory . --capture --output-file coverage.info
                # generate report
                COMMAND ${GENHTML} --rc genhtml_branch_coverage=1 --demangle-cpp -o coverage coverage.info
                WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
        set(YGG_BUILD_FLAGS "${YGG_BUILD_FLAGS} --coverage -fno-inline -fno-inline-small-functions -fno-default-inline -fpermissive")
    endif(YGG_ENABLE_COVERAGE)
endif()
add_link_options(-L/opt/anaconda3/envs/Yggdrasil/lib)
#message(${YGG_BUILD_FLAGS})
set(CMAKE_CXX_FLAGS_DEBUG "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")
set(CMAKE_CXX_FLAGS_RELEASE "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")

