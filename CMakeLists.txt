cmake_minimum_required(VERSION 3.16)
set(CMAKE_VERBOSE_MAKEFILE True)
if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)
if (POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   if("$ENV{SDKROOT}")
      SET(CMAKE_OSX_SYSROOT "$ENV{SDKROOT}")
      # SET(CMAKE_OSX_SYSROOT /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk)
   endif()
endif()

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules)

set(LIB_MAJOR_VERSION "0")
set(LIB_MINOR_VERSION "1")
set(LIB_PATCH_VERSION "0")
set(LIB_VERSION_STRING "${LIB_MAJOR_VERSION}.${LIB_MINOR_VERSION}.${LIB_PATCH_VERSION}")

if (CMAKE_VERSION VERSION_LESS 3.0)
    PROJECT(YggInterfaceCPP CXX)
else()
    cmake_policy(SET CMP0048 NEW)
    PROJECT(YggInterfaceCPP VERSION "${LIB_VERSION_STRING}" LANGUAGES CXX)
endif()


if(POLICY CMP0094)  # https://cmake.org/cmake/help/latest/policy/CMP0094.html
    cmake_policy(SET CMP0094 NEW)  # FindPython should return the first matching Python
endif()

# compile in release with debug info mode by default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(YGG_BUILD_TESTS "Build tests" ON)
option(YGG_BUILD_THIRDPARTY_GTEST "Use gtest installation in 'thirdpart/gtest' by default if available" ON)
option(YGG_BUILD_ASAN "Build with address sanitizer (gcc/clang)" OFF)
option(YGG_BUILD_UBSAN "Build with undefined behavior sanitizer (gcc/clang)" OFF)
option(YGG_ENABLE_INSTRUMENTATION_OPT "Build yggdrasil with -march or -mcpu options" ON)

# option(YGG_BUILD_THIRDPARTY_GTEST
#     "Use gtest installation in `thirdparty/gtest` by default if available" OFF)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
    if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Qunused-arguments -fcolor-diagnostics")
    endif()
endif(CCACHE_FOUND)

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    if(YGG_ENABLE_INSTRUMENTATION_OPT AND NOT CMAKE_CROSSCOMPILING)
        if(CMAKE_SYSTEM_PROCESSOR STREQUAL "powerpc" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc64le")
          set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mcpu=native")
        else()
          #FIXME: x86 is -march=native, but doesn't mean every arch is this option. To keep original project's compatibility, I leave this except POWER.
          set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native")
        endif()
    endif()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror")
    set(EXTRA_CXX_FLAGS -Weffc++ -Wswitch-default -Wfloat-equal -Wconversion -Wsign-conversion)
    # if (YGG_BUILD_CXXMIN)
    #    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++03 -Wc++0x-compat -Werror=unused-parameter")
    # elseif (YGG_BUILD_CXX11 AND CMAKE_VERSION VERSION_LESS 3.1)
    #     if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.7.0")
    #         set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
    #     else()
    #         set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    #     endif()
    # elseif (YGG_BUILD_CXX17 AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS "5.0")
    #     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
    # endif()
    if (YGG_BUILD_ASAN)
        if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.8.0")
            message(FATAL_ERROR "GCC < 4.8 doesn't support the address sanitizer")
        else()
            add_compile_options(-fsanitize=address)
            add_link_options(-fsanitize=address)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
        endif()
    endif()
    if (YGG_BUILD_UBSAN)
        if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.9.0")
            message(FATAL_ERROR "GCC < 4.9 doesn't support the undefined behavior sanitizer")
        else()
            add_compile_options(-fsanitize=undefined)
            add_link_options(-fsanitize=undefined)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined")
        endif()
    endif()
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    if(NOT CMAKE_CROSSCOMPILING)
      if(CMAKE_SYSTEM_PROCESSOR STREQUAL "powerpc" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc64le")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mcpu=native")
      else()
        if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
	  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mcpu=apple-m1")
	else()
          #FIXME: x86 is -march=native, but doesn't mean every arch is this option. To keep original project's compatibility, I leave this except POWER.
          set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native")
        endif()
      endif()
    endif()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -Wno-missing-field-initializers")
    set(EXTRA_CXX_FLAGS -Weffc++ -Wswitch-default -Wfloat-equal -Wconversion -Wimplicit-fallthrough)
    # if (YGG_BUILD_CXXMIN)
    #    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++03 -Wc++0x-compat -Werror=unused-parameter")
    # elseif (YGG_BUILD_CXX11 AND CMAKE_VERSION VERSION_LESS 3.1)
    #     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    # elseif (YGG_BUILD_CXX17 AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.0")
    #     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
    # endif()
    if (YGG_BUILD_ASAN)
        add_compile_options(-fsanitize=address)
        add_link_options(-fsanitize=address)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
    endif()
    if (YGG_BUILD_UBSAN)
        if (CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
            add_compile_options(-fsanitize=undefined-trap -fsanitize-undefined-trap-on-error)
            add_link_options(-fsanitize=undefined-trap -fsanitize-undefined-trap-on-error)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined-trap -fsanitize-undefined-trap-on-error")
        else()
            add_compile_options(-fsanitize=undefined)
            add_link_options(-fsanitize=undefined)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined")
        endif()
    endif()
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    add_definitions(-D_CRT_SECURE_NO_WARNINGS=1)
    add_definitions(-DNOMINMAX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc /bigobj /Zm10")
    # CMake >= 3.10 should handle the above CMAKE_CXX_STANDARD fine, otherwise use /std:c++XX with MSVC >= 19.10
    # if (YGG_BUILD_CXXMIN)
    #    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++03 /Wc++0x-compat")
    # elseif (YGG_BUILD_CXX11 AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS "19.10")
    #     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++14") # c++11 not allowed
    # elseif (YGG_BUILD_CXX17 AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS "19.14")
    #     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++17")
    # endif()
    # Always compile with /WX
    if(CMAKE_CXX_FLAGS MATCHES "/WX-")
        string(REGEX REPLACE "/WX-" "/WX" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /WX")
    endif()
elseif (CMAKE_CXX_COMPILER_ID MATCHES "XL")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -qarch=auto")
endif()

#add extra search paths for libraries and includes
SET(INCLUDE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/include" CACHE PATH "The directory the headers are installed in")
SET(LIB_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib" CACHE STRING "Directory where lib will install")

IF(UNIX OR CYGWIN)
    SET(_CMAKE_INSTALL_DIR "${LIB_INSTALL_DIR}/cmake/${PROJECT_NAME}")
ELSEIF(WIN32)
    SET(_CMAKE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/cmake")
ENDIF()
SET(CMAKE_INSTALL_DIR "${_CMAKE_INSTALL_DIR}" CACHE PATH "The directory cmake files are installed in")

ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS)
include(FetchContent)

include_directories(communication)

# pkg-config
# IF (UNIX OR CYGWIN)
#   CONFIGURE_FILE (${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.pc.in
#                   ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc
#                   @ONLY)
#   INSTALL (FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc
#       DESTINATION "${LIB_INSTALL_DIR}/pkgconfig"
#       COMPONENT pkgconfig)
# ENDIF()

# install(DIRECTORY include/rapidjson
#     DESTINATION "${INCLUDE_INSTALL_DIR}"
#     COMPONENT dev)

################################################################################
# # Export package for use from the build tree
# EXPORT( PACKAGE ${PROJECT_NAME} )

# # Create the RapidJSONConfig.cmake file for other cmake projects.
# # ... for the build tree
# SET( CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
# SET( CONFIG_DIR ${CMAKE_CURRENT_BINARY_DIR})
# SET( ${PROJECT_NAME}_INCLUDE_DIR "\${${PROJECT_NAME}_SOURCE_DIR}/include" )

# CONFIGURE_FILE( ${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}Config.cmake.in
#     ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake @ONLY )
# CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}ConfigVersion.cmake.in
#     ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake @ONLY)

# # ... for the install tree
# SET( CMAKECONFIG_INSTALL_DIR ${LIB_INSTALL_DIR}/cmake/${PROJECT_NAME} )
# FILE( RELATIVE_PATH REL_INCLUDE_DIR
#     "${CMAKECONFIG_INSTALL_DIR}"
#     "${CMAKE_INSTALL_PREFIX}/include" )

# SET( ${PROJECT_NAME}_INCLUDE_DIR "\${${PROJECT_NAME}_CMAKE_DIR}/${REL_INCLUDE_DIR}" )
# SET( CONFIG_SOURCE_DIR )
# SET( CONFIG_DIR )
# CONFIGURE_FILE( ${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}Config.cmake.in
#     ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${PROJECT_NAME}Config.cmake @ONLY )

# INSTALL(FILES "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${PROJECT_NAME}Config.cmake"
#         DESTINATION ${CMAKECONFIG_INSTALL_DIR} )

# # Install files
# IF(CMAKE_INSTALL_DIR)
#   INSTALL(FILES
#       ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
#       ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
#       DESTINATION "${CMAKE_INSTALL_DIR}"
#       COMPONENT dev)
# ENDIF()

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
    message(FATAL_ERROR "In-source build prohibited.")
endif("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")

file(GLOB SOURCES "communication/utils/*.cpp" "communication/communicators/*.cpp")
add_library(YggInterface SHARED ${SOURCES})


add_definitions(-DRAPIDJSON_YGGDRASIL)
add_definitions(-DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION)
add_definitions(-D_USE_MATH_DEFINES)

option(BUILD_PYTHON_LIBRARY "Build the Yggdrasil Python interface library" ON)
#########################################
if(BUILD_PYTHON_LIBRARY)
    if(POLICY CMP0094)  # https://cmake.org/cmake/help/latest/policy/CMP0094.html
        cmake_policy(SET CMP0094 NEW)  # FindPython should return the first matching Python
    endif()
    add_definitions(-DWITH_PYTHON)
    find_package(PythonInterp 3)
    find_package(PythonLibs 3 REQUIRED)
    find_package(Boost REQUIRED COMPONENTS python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR} log)

    #find_package(Python3 COMPONENTS Interpreter Development NumPy REQUIRED)
    #include_directories(${Python_INCLUDE_DIRS})
    #include_directories(${Python3_NumPy_INCLUDE_DIRS})
    #set(YGG_PYTHON_ENV "DATADIR=${CMAKE_SOURCE_DIR}/test;YGG_PYTHON_EXEC=${Python3_EXECUTABLE}")

    # There is an issue with embedding Python on Windows in conda environments
    # https://github.com/ContinuumIO/anaconda-issues/issues/11374
    #if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    #    set(YGG_PYTHON_ENV "${YGG_PYTHON_ENV};PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
    #endif()

    #target_link_libraries(YggInterface ${PYTHON_LIBRARIES} Python3::NumPy)


    file(GLOB_RECURSE PYSOURCES ${CMAKE_SOURCE_DIR}/communication/python/*.cpp)
    add_library(pyygg MODULE ${SOURCES} ${PYSOURCES})
    target_include_directories(pyygg PRIVATE ${PYTHON_INCLUDE_DIRS} ${CMAKE_SOURCE_DIR}/communication/python)
    target_link_libraries(pyygg boost_python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR})
    message("PYTHON")

else()
    find_package(Boost REQUIRED COMPONENTS log)
endif()
##############################

#file(GLOB_RECURSE SOURCES "communication/**/*.cpp")

option(YGG_BUILD_TESTS "Build tests" OFF)
option(YGG_BUILD_THIRDPARTY_GTEST "Use gtest installation in 'thirdpart/gtest' by default if available" ON)


set(YGG_BUILD_FLAGS "")

##########
# OpenMP #
##########

find_package(OpenMP)
if (OpenMP_CXX_FOUND)
    message(STATUS "OpenMP found")
    target_link_libraries(YggInterface OpenMP::OpenMP_CXX)
    if(BUILD_PYTHON_LIBRARY)
        target_link_libraries(pyygg OpenMP::OpenMP_CXX)
    endif()
else()
    message(STATUS "NO OpenMP")
endif()

#######
# MPI #
#######

find_package(MPI)
if(MPI_FOUND)
    message(STATUS "MPI found")
    add_definitions(-DMPIINSTALLED)
    include_directories(${MPI_INCLUDE_PATH})
    target_link_libraries(YggInterface ${MPI_C_LIBRARIES} MPI::MPI_CXX)
    if(BUILD_PYTHON_LIBRARY)
        target_link_libraries(pyygg ${MPI_C_LIBRARIES} MPI::MPI_CXX)
    endif()
    # include_directories(MPI_CXX_INCLUDE_DIRS)
    # target_link_libraries(YggInterface MPI::MPI_CXX)
    # if(BUILD_PYTHON_LIBRARY)
    #     target_link_libraries(pyygg MPI::MPI_CXX)
    # endif()
else()
    message(STATUS "NO MPI")
ENDIF()

##########
# Python #
##########

option(YGGDRASIL_DISABLE_PYTHON_C_API "Disable the Python C API" OFF)
if(YGGDRASIL_DISABLE_PYTHON_C_API)
    add_definitions(-DYGGDRASIL_DISABLE_PYTHON_C_API)
else()
    # needed on GitHub Actions CI: actions/setup-python does not touch registry/frameworks on Windows/macOS
    # this mirrors PythonInterp behavior which did not consult registry/frameworks first
    if (NOT DEFINED Python_FIND_REGISTRY)
        set(Python_FIND_REGISTRY "LAST")
    endif ()
    if (NOT DEFINED Python_FIND_FRAMEWORK)
        set(Python_FIND_FRAMEWORK "LAST")
    endif ()

    message(STATUS "Python executable is ${Python3_EXECUTABLE}")
    if(NOT Python3_ROOT_DIR)
        set(CONDA_PREFIX "$ENV{CONDA_PREFIX}")
        if(CONDA_PREFIX)
            message(STATUS "CONDA_PREFIX = ${CONDA_PREFIX}")
            set(Python3_ROOT_DIR "${CONDA_PREFIX}")
        else()
            if(Python3_EXECUTABLE)
                exec_program(${Python3_EXECUTABLE}
	                     ARGS "-c \"import sysconfig; print(sysconfig.get_config_var('base'))\""
		             OUTPUT_VARIABLE PYTHON_ROOT
		             RETURN_VALUE ROOT_NOT_FOUND)
                if(ROOT_NOT_FOUND)
     	            message(FATAL_ERROR "Python root not found")
                endif()
	        set(Python3_ROOT_DIR "${RETURN_VALUE}")
            endif()
         endif()
    endif()
    if(Python3_ROOT_DIR)
        message(STATUS "Python root directory is ${Python3_ROOT_DIR}")
    endif()
    find_package(Python3 COMPONENTS Interpreter Development NumPy REQUIRED)
    if(NOT Python3_NumPy_FOUND)
        message(FATAL_ERROR "NumPy headers not found")
    endif()
    if(NOT Python3_FOUND)
        message(FATAL_ERROR "Python libraries not found")
    endif()

    # Set environment variables required for embedding Python
    set(RAPIDJSON_PYTHON_ENV "DATADIR=${CMAKE_SOURCE_DIR}/test;YGG_PYTHON_EXEC=${Python3_EXECUTABLE}")

    # There is an issue with embedding Python on Windows in conda environments
    # https://github.com/ContinuumIO/anaconda-issues/issues/11374
    if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        set(RAPIDJSON_PYTHON_ENV "${RAPIDJSON_PYTHON_ENV};PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
    endif()
    
    target_link_libraries(YggInterface ${Python3_LIBRARIES} Python3::NumPy)
    target_link_libraries(YggInterface ${Python3_NumPy_LIBRARIES})
    include_directories(${Python3_INCLUDE_DIRS} ${Python3_NumPy_INCLUDE_DIRS})
endif()

#########
# Boost #
#########

find_package(Boost REQUIRED COMPONENTS log)
include_directories(${Boost_INCLUDE_DIRS})
add_definitions(-DBOOST_LOG_DYN_LINK)

target_link_libraries(YggInterface ${Boost_LOG_LIBRARY})
if (Boost_DATE_TIME_FOUND)
    target_link_libraries(YggInterface ${Boost_DATE_TIME_LIBRARY})
else()
    add_definitions(-DBOOST_DATE_TIME_NO_LIB)
endif()

#############
# Rapidjson #
#############

set(RAPIDJSON_INCLUDE_DIRS "${CMAKE_SOURCE_DIR}/communication/rapidjson/include")
include_directories(${RAPIDJSON_INCLUDE_DIRS})
message(STATUS "Rapidjson found ${RAPIDJSON_INCLUDE_DIRS}")

##########
# ZeroMQ #
##########

option(ENABLE_ZMQ "Enable zeromq" ON)
if(ENABLE_ZMQ)
    if (CONDA_PREFIX)
        find_package(ZeroMQ REQUIRED)
    else()
        # Only conda version has CMake config
        ## load in pkg-config support
        find_package(PkgConfig)
        ## use pkg-config to get hints for 0mq locations
        pkg_check_modules(PC_ZeroMQ QUIET zmq)

        ## use the hint from above to find where 'zmq.h' is located
        find_path(ZeroMQ_INCLUDE_DIR
            NAMES zmq.h
            PATHS ${PC_ZeroMQ_INCLUDE_DIRS}
            )

        ## use the hint from above to find the location of libzmq
        find_library(ZeroMQ_LIBRARY
            NAMES zmq
            PATHS ${PC_ZeroMQ_LIBRARY_DIRS}
            )
	if ((NOT ZeroMQ_INCLUDE_DIR STREQUAL ZeroMQ_INCLUDE_DIR-NOTFOUND) AND (NOT ZeroMQ_LIBRARY STREQUAL ZeroMQ_LIBRARY-NOTFOUND))
            set(ZeroMQ_FOUND 1)
	endif()
    endif()
else (ENABLE_ZMQ)
    message(STATUS "No zmq support will be built." )
endif (ENABLE_ZMQ)

if (ZeroMQ_FOUND)
    message(STATUS "ZeroMQ found ZeroMQ_INCLUDE_DIR = ${ZeroMQ_INCLUDE_DIR}")
    add_definitions(-DZMQINSTALLED -DZMQ_BUILD_DRAFT_API)

    include_directories(${ZeroMQ_INCLUDE_DIR})
    target_link_libraries(YggInterface ${ZeroMQ_LIBRARY})
    if(BUILD_PYTHON_LIBRARY)
        target_link_libraries(pyygg ${ZeroMQ_LIBRARY})
    endif()
else()
    message(STATUS "NO ZeroMQ")
endif()

##################
# IPC dependency #
##################

option(USING_IPC "Default to using IPC V queues" OFF)
if (NOT ZeroMQ_FOUND)
    set(USING_IPC ON)
endif()
if(USING_IPC)
    if (UNIX OR CYGWIN)
        add_definitions(-DIPCDEF)
    else()
        set(USING_IPC OFF)
    endif()
endif()
if (UNIX OR CYGWIN)
    add_definitions(-DIPCINSTALLED)
endif()


###################
# Version options #
###################

if(CONDA_PREFIX)
    add_link_options("-L${CONDA_PREFIX}/lib")
endif(CONDA_PREFIX)

if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
   set(CMAKE_CXX_FLAGS_DEBUG "/Zi -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELEASE "/Zi -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/Zi -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
else ()
   set(CMAKE_CXX_FLAGS_DEBUG "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")
   set(CMAKE_CXX_FLAGS_RELEASE "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} -fpermissive")
endif ()

#########
# Tests #
#########

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

