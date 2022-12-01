cmake_minimum_required(VERSION 3.5)

if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)

project(YggInterfaceCPP VERSION 0.1.0.0)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS)



if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
    message(FATAL_ERROR "In-source build prohibited.")
endif("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")

#file(GLOB_RECURSE SOURCES "communication/**/*.cpp")
file(GLOB SOURCES "communication/utils/*.cpp" "communication/datatypes/*.cpp")
option(YGG_BUILD_TESTS "Build tests" ON)
option(YGG_BUILD_THIRDPARTY_GTEST "Use gtest installation in 'thirdpart/gtest' by default if available" ON)

set(YGG_BUILD_FLAGS "")
#find cppzmq wrapper, installed by make of cppzmq
find_package(cppzmq)
########################################################################
# LIBZMQ dependency
########################################################################
#find_package(libzmq)
#IF (libzmq_FOUND)
#    include_directories(${libzmq_INCLUDE_DIRS})
#    list(APPEND MORE_LIBRARIES ${libzmq_LIBRARIES})
#    IF (PC_LIBZMQ_FOUND)
#        set(pkg_config_names_private "${pkg_config_names_private} libzmq")
#        list(APPEND OPTIONAL_LIBRARIES_STATIC ${PC_LIBZMQ_STATIC_LDFLAGS})
#    ELSE (PC_LIBZMQ_FOUND)
#        set(pkg_config_libs_private "${pkg_config_libs_private} -lzmq")
#    ENDIF (PC_LIBZMQ_FOUND)
#ELSE (libzmq_FOUND)
#    message("libzmq not found." )
#ENDIF (libzmq_FOUND)
IF (cppzmq_FOUND)
    message("Found cppzmq   ${ZeroMQ_INCLUDE_DIRS}  XX")
    include_directories("/opt/anaconda3/include")
    add_definitions(-DZMQ_BUILD_DRAFT_API)
ENDIF()

find_package(Boost COMPONENTS log REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})
add_definitions(-DBOOST_LOG_DYN_LINK)

########################################################################
# LIBCZMQ dependency
########################################################################
find_package(libczmq)
IF (libczmq_FOUND)
    include_directories(${libczmq_INCLUDE_DIRS})
    list(APPEND MORE_LIBRARIES ${libczmq_LIBRARIES})
    IF (PC_LIBCZMQ_FOUND)
        set(pkg_config_names_private "${pkg_config_names_private} libczmq")
        list(APPEND OPTIONAL_LIBRARIES_STATIC ${PC_LIBCZMQ_STATIC_LDFLAGS})
    ELSE (PC_LIBCZMQ_FOUND)
        set(pkg_config_libs_private "${pkg_config_libs_private} -lczmq")
    ENDIF (PC_LIBCZMQ_FOUND)
    add_definitions(-DZMQINSTALLED)
ELSE (libczmq_FOUND)
    message("libczmq not found." )
ENDIF (libczmq_FOUND)

find_package(OpenMP)
if (OpenMP_FOUND)
    add_definitions(-D_OPENMP -DMPIINSTALLED)
endif()

find_package(Python3 COMPONENTS Interpreter Development NumPy REQUIRED)
include_directories(${Python_INCLUDE_DIRS})
include_directories(${Python3_NumPy_INCLUDE_DIRS})
set(YGG_PYTHON_ENV "DATADIR=${CMAKE_SOURCE_DIR}/test;YGG_PYTHON_EXEC=${Python3_EXECUTABLE}")

# There is an issue with embedding Python on Windows in conda environments
# https://github.com/ContinuumIO/anaconda-issues/issues/11374
if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(YGG_PYTHON_ENV "${YGG_PYTHON_ENV};PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
endif()

add_library(YggInterface SHARED ${SOURCES})
target_link_libraries(YggInterface ${PYTHON_LIBRARIES} Python3::NumPy)
message("X ${Boost_LOG_LIBRARY}")
target_link_libraries(YggInterface ${Boost_LOG_LIBRARY})
include_directories("communication")

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
        add_definitions(-DIPCINSTALLED)
    endif()
ELSE(WIN32)
    if (libzmq_FOUND)
        target_link_libraries(YggInterface ${libzmq_LIBRARIES})
    endif()
    if (libczmq_FOUND)
        target_link_libraries(YggInterface ${libczmq_LIBRARIES})
        add_definitions(-DIPCINSTALLED)
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
message(${YGG_BUILD_FLAGS})
set(CMAKE_CXX_FLAGS_DEBUG "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")
set(CMAKE_CXX_FLAGS_RELEASE "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")

