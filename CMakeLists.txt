cmake_minimum_required(VERSION 3.16)
set(CMAKE_VERBOSE_MAKEFILE ON)
if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)
# if (POLICY CMP0148)
#     cmake_policy(SET CMP0148 NEW)
# endif (POLICY CMP0148)
if (POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif()

if (APPLE)
   if("$ENV{SDKROOT}")
      SET(CMAKE_OSX_SYSROOT "$ENV{SDKROOT}")
      # SET(CMAKE_OSX_SYSROOT /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk)
   endif()
endif()

SET(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules")

set(LIB_MAJOR_VERSION "0")
set(LIB_MINOR_VERSION "1")
set(LIB_PATCH_VERSION "0")
set(LIB_VERSION_STRING "${LIB_MAJOR_VERSION}.${LIB_MINOR_VERSION}.${LIB_PATCH_VERSION}")

if (CMAKE_VERSION VERSION_LESS 3.0)
    PROJECT(YggInterfaceCPP)
else()
    cmake_policy(SET CMP0048 NEW)
    PROJECT(YggInterfaceCPP VERSION "${LIB_VERSION_STRING}")
endif()

if (UNIX)
    include(GNUInstallDirs)
endif()
if (SKBUILD AND WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
endif()

if(POLICY CMP0094)  # https://cmake.org/cmake/help/latest/policy/CMP0094.html
    cmake_policy(SET CMP0094 NEW)  # FindPython should return the first matching Python
endif()

# compile in release with debug info mode by default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()

if (WIN32)
   set(CMAKE_CXX_EXTENSIONS OFF)
endif()
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(BUILD_CPP_LIBRARY "Build the Yggdrasil C++ interface library" ON)
option(BUILD_PYTHON_LIBRARY "Build the Yggdrasil Python interface library" OFF)
option(YGG_PYTHON_LIBRARY_WRAP "Build the Yggdrasil Python interface library by wrapping the C++ module" ON)
option(BUILD_FORTRAN_LIBRARY "Build the Yggdrasil Fortran interface library" OFF)
option(DONT_CHECK_FORTRAN_C_COMPAT "Disable verification of Fortran/C/CXX compiler compatibility" OFF)
option(ENABLE_ZMQ "Enable zeromq" ON)
option(USING_IPC "Default to using IPC V queues" OFF)
option(YGGDRASIL_DISABLE_PYTHON_C_API "Disable the Python C API" OFF)
option(ENABLE_OMP "Enable OpenMP" ON)
option(ENABLE_MPI "Enable MPI" ON)
option(ENABLE_THREADS "Enable std::threads" ON)
option(YGG_BUILD_TESTS "Build tests" OFF)
option(YGG_BUILD_FORTRAN_TESTS "Build Fortran tests" OFF)
option(YGG_BUILD_CXX_TESTS "Build C/C++ tests" OFF)
option(YGG_ENABLE_COVERAGE "Enable coverage reporting" OFF)
option(YGG_BUILD_THIRDPARTY_GTEST "Use gtest installation in 'thirdpart/gtest' by default if available" ON)
option(YGG_BUILD_ASAN "Build with address sanitizer (gcc/clang)" OFF)
option(YGG_BUILD_UBSAN "Build with undefined behavior sanitizer (gcc/clang)" OFF)
option(YGG_ENABLE_INSTRUMENTATION_OPT "Build yggdrasil with -march or -mcpu options" ON)

message(STATUS "CMAKE_CXX_FLAGS = ${CMAKE_CXX_FLAGS}")

if(NOT WIN32)
    find_program(CCACHE_FOUND ccache)
    if(CCACHE_FOUND)
        set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
        set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
        if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Qunused-arguments -fcolor-diagnostics")
        endif()
    endif(CCACHE_FOUND)
endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    if(YGG_ENABLE_INSTRUMENTATION_OPT AND NOT CMAKE_CROSSCOMPILING)
        if(CMAKE_SYSTEM_PROCESSOR STREQUAL "powerpc" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc64le")
          set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mcpu=native")
        else()
          #FIXME: x86 is -march=native, but doesn't mean every arch is this option. To keep original project's compatibility, I leave this except POWER.
          set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native")
        endif()
    endif()
    if (WIN32)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wa,-mbig-obj")
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


set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
    message(FATAL_ERROR "In-source build prohibited.")
endif("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")


add_definitions(-DRAPIDJSON_YGGDRASIL)
add_definitions(-DRAPIDJSON_HAS_STDSTRING)
add_definitions(-DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION)
add_definitions(-D_USE_MATH_DEFINES)

set(RAPIDJSON_PYTHON_ENV "YGG_MODEL_INDEX=123")

set(YGG_BUILD_FLAGS "")

#########################################
# DEPENDENCIES
#########################################

cmake_path(SET CONDA_PREFIX "$ENV{CONDA_PREFIX}")
if(CONDA_PREFIX)
    message(STATUS "CONDA_PREFIX = ${CONDA_PREFIX}")
endif()

#############
# Rapidjson #
#############

if(NOT RAPIDJSON_INCLUDE_DIRS)
    set(RAPIDJSON_INCLUDE_DIRS "${CMAKE_SOURCE_DIR}/communication/rapidjson/include")
endif()
set(DEPS_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS} ${RAPIDJSON_INCLUDE_DIRS})
message(STATUS "Rapidjson found ${RAPIDJSON_INCLUDE_DIRS}")

##############################

##########
# ZeroMQ #
##########

if(ENABLE_ZMQ)
    find_package(ZeroMQ CONFIG)
    if (NOT ZeroMQ_FOUND)
        message(STATUS "ZeroMQ could not be found using default search tree. Trying via pkg-config...")
        # Only conda version has CMake config
        ## load in pkg-config support
        find_package(PkgConfig)
        ## use pkg-config to get hints for 0mq locations
        pkg_check_modules(PC_ZeroMQ QUIET zmq libzmq libzmq-static)

        if (CONDA_PREFIX)
	    if (WIN32)
                set(PC_ZeroMQ_INCLUDE_DIRS "${CONDA_PREFIX}/Library/include ${PC_ZeroMQ_INCLUDE_DIRS}")
                set(PC_ZeroMQ_LIBRARY_DIRS "${CONDA_PREFIX}/Library/lib ${CONDA_PREFIX}/Library/bin ${PC_ZeroMQ_LIBRARY_DIRS}")
            else()
                set(PC_ZeroMQ_INCLUDE_DIRS "${CONDA_PREFIX}/include ${PC_ZeroMQ_INCLUDE_DIRS}")
                set(PC_ZeroMQ_LIBRARY_DIRS "${CONDA_PREFIX}/lib ${PC_ZeroMQ_LIBRARY_DIRS}")
            endif()
        endif()

        ## use the hint from above to find where 'zmq.h' is located
        find_path(ZeroMQ_INCLUDE_DIR
            NAMES zmq.h
            PATHS ${PC_ZeroMQ_INCLUDE_DIRS}
            )

        ## use the hint from above to find the location of libzmq
        find_library(ZeroMQ_LIBRARY_NEW
            NAMES zmq libzmq libzmq-static
            PATHS ${PC_ZeroMQ_LIBRARY_DIRS}
            )
        set(ZeroMQ_LIBRARY ${ZeroMQ_LIBRARY_NEW})
        message(STATUS "PkgConfig ZeroMQ_INCLUDE_DIR = ${ZeroMQ_INCLUDE_DIR}")
	message(STATUS "PkgConfig ZeroMQ_LIBRARY = ${ZeroMQ_LIBRARY}")
	if ((NOT ZeroMQ_INCLUDE_DIR STREQUAL ZeroMQ_INCLUDE_DIR-NOTFOUND) AND (NOT ZeroMQ_LIBRARY STREQUAL ZeroMQ_LIBRARY-NOTFOUND))
            set(ZeroMQ_FOUND 1)
	endif()
    endif()
    # Force error
    if ((NOT ZeroMQ_FOUND) AND (WIN32))
        find_package(ZeroMQ CONFIG REQUIRED)
    endif()
else (ENABLE_ZMQ)
    message(STATUS "No zmq support will be built." )
endif (ENABLE_ZMQ)

if (ZeroMQ_FOUND)
    message(STATUS "ZeroMQ_LIBRARY = ${ZeroMQ_LIBRARY}")
    message(STATUS "ZeroMQ_LIBRARIES = ${ZeroMQ_LIBRARIES}")
    message(STATUS "ZeroMQ_INCLUDE_DIR = ${ZeroMQ_INCLUDE_DIR}")
    if (WIN32 AND ZeroMQ_LIBRARY)
        string(FIND ${ZeroMQ_LIBRARY} ".dll" ZeroMQ_DLL_POS)
        if (NOT "${ZeroMQ_DLL_POS}" STREQUAL "-1")
            get_filename_component(ZeroMQ_BIN_DIR ${ZeroMQ_LIBRARY} DIRECTORY)
            get_filename_component(ZeroMQ_PREFIX_DIR ${ZeroMQ_BIN_DIR} DIRECTORY)
            get_filename_component(ZeroMQ_BASE_NAME ${ZeroMQ_LIBRARY} NAME_WLE)
            message(STATUS "ZeroMQ_PREFIX_DIR = ${ZeroMQ_PREFIX_DIR}")
            message(STATUS "ZeroMQ_BASE_NAME = ${ZeroMQ_BASE_NAME}")
            find_library(ZeroMQ_LIBRARY_LIB
                NAMES ${ZeroMQ_BASE_NAME}
                PATHS ${ZeroMQ_PREFIX_DIR}/lib
                )
            set(ZeroMQ_LIBRARY ${ZeroMQ_LIBRARY_LIB})
	    message(STATUS "Updated ZeroMQ_LIBRARY = ${ZeroMQ_LIBRARY}")
        endif()
    endif()
    add_definitions(-DZMQINSTALLED)
    set(DEPS_LIBRARIES ${DEPS_LIBRARIES} ${ZeroMQ_LIBRARY})
    if (NOT ZeroMQ_LIBRARY)
        set(DEPS_LIB_NAMES ${DEPS_LIB_NAMES} zmq)
    endif()
    set(DEPS_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS} ${ZeroMQ_INCLUDE_DIR})
else()
    message(STATUS "NO ZeroMQ")
endif()

##########
# OpenMP #
##########

if (ENABLE_OMP)
find_package(OpenMP)
if (OpenMP_CXX_FOUND)
    message(STATUS "OpenMP found")
    set(DEPS_LIBRARIES ${DEPS_LIBRARIES} ${OpenMP_CXX_LIBRARIES})
    set(DEPS_LIB_NAMES ${DEPS_LIB_NAMES} ${OpenMP_CXX_LIB_NAMES})
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    message(STATUS "OpenMP_CXX_LIBRARIES = ${OpenMP_CXX_LIBRARIES}")
    message(STATUS "OpenMP_CXX_LIB_NAMES = ${OpenMP_CXX_LIB_NAMES}")
    message(STATUS "OpenMP_C_FLAGS = ${OpenMP_C_FLAGS}")
    message(STATUS "OpenMP_CXX_FLAGS = ${OpenMP_CXX_FLAGS}")
else()
    message(STATUS "NO OpenMP")
endif()
endif()

#############
# Threading #
#############

set(THREADS_PREFER_PTHREAD_FLAG TRUE)
set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
if(ENABLE_THREADS)
find_package(Threads)
if (Threads_FOUND)
    add_definitions(-DTHREADSINSTALLED)
    set(DEPS_LIB_NAMES ${DEPS_LIB_NAMES} Threads::Threads)
    if (CMAKE_THREAD_LIBS_INIT)
        set(DEPS_LIB_NAMES ${DEPS_LIB_NAMES} ${CMAKE_THREAD_LIBS_INIT})
    endif()
    if (CMAKE_USE_PTHREADS_INIT)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
    endif()
else()
    message(STATUS "NO Threads")
endif()
endif()

#######
# MPI #
#######

if(ENABLE_MPI)
find_package(MPI)
if(MPI_CXX_FOUND)
    message(STATUS "MPI found")
    add_definitions(-DMPIINSTALLED)
    set(DEPS_LIBRARIES ${DEPS_LIBRARIES} ${MPI_CXX_LIBRARIES})
    set(DEPS_LIB_NAMES ${DEPS_LIB_NAMES} MPI::MPI_CXX)
    set(DEPS_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS} ${MPI_CXX_INCLUDE_DIRS})
else()
    message(STATUS "NO MPI")
endif()
endif()

##########
# Python #
##########

set(DEPS_LIBRARIES_NO_PYTHON ${DEPS_LIBRARIES})
set(DEPS_LIB_NAMES_NO_PYTHON ${DEPS_LIB_NAMES})
if(YGGDRASIL_DISABLE_PYTHON_C_API)
    add_definitions(-DYGGDRASIL_DISABLE_PYTHON_C_API)
else()
    # needed on GitHub Actions CI: actions/setup-python does not touch registry/frameworks on Windows/macOS
    # this mirrors PythonInterp behavior which did not consult registry/frameworks first
    if (NOT DEFINED Python3_FIND_REGISTRY)
        set(Python3_FIND_REGISTRY "LAST")
    endif ()
    if (NOT DEFINED Python3_FIND_FRAMEWORK)
        set(Python3_FIND_FRAMEWORK "LAST")
    endif ()
    if(Python3_EXECUTABLE)
        message(STATUS "Python executable is ${Python3_EXECUTABLE}")
        if(NOT Python3_NumPy_INCLUDE_DIRS)
            exec_program(${Python3_EXECUTABLE}
	                 ARGS "-c \"import numpy; print(numpy.get_include())\""
		         OUTPUT_VARIABLE Python3_NumPy_INCLUDE_DIRS
		         RETURN_VALUE NUMPY_NOT_FOUND)
            if(NUMPY_NOT_FOUND)
                message(FATAL_ERROR "Numpy include dirs not found")
            endif()
        endif()
    else()
        if(NOT Python3_ROOT_DIR)
            if(CONDA_PREFIX)
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
	            set(Python3_ROOT_DIR "${PYTHON_ROOT}")
                endif()
            endif()
        endif()
    endif()
    if(Python3_EXECUTABLE OR Python3_ROOT_DIR)
        # Force use of specified installation, should be enabled by
	# default for CMP0094=NEW and CMake >= 3.15
        if(NOT Python3_FIND_STRATEGY)
            set(Python3_FIND_STRATEGY LOCATION)
        endif()
    endif()
    if(Python3_ROOT_DIR)
        message(STATUS "Python root directory is ${Python3_ROOT_DIR}")
        if (NOT Python3_ROOT)
            set(Python3_ROOT ${Python3_ROOT_DIR})
        endif()
    endif()
    find_package(Python3 COMPONENTS Interpreter Development NumPy REQUIRED)
    if(NOT Python3_NumPy_FOUND)
        message(FATAL_ERROR "NumPy headers not found")
    endif()
    if(NOT Python3_FOUND)
        message(FATAL_ERROR "Python libraries not found")
    endif()

    # Set environment variables required for embedding Python
    # set(RAPIDJSON_PYTHON_ENV "${RAPIDJSON_PYTHON_ENV};DATADIR=${CMAKE_SOURCE_DIR}/test;YGG_PYTHON_EXEC=${Python3_EXECUTABLE}")

    # There is an issue with embedding Python on Windows in conda environments
    # https://github.com/ContinuumIO/anaconda-issues/issues/11374
    if(WIN32)
        set(RAPIDJSON_PYTHON_ENV2 "PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
    endif()
    
    set(DEPS_LIBRARIES ${DEPS_LIBRARIES} ${Python3_LIBRARIES})
    set(DEPS_LIB_NAMES ${DEPS_LIB_NAMES} Python3::NumPy)
    set(DEPS_LIBRARIES_PYTHON ${Python3_LIBRARIES})
    set(DEPS_LIB_NAMES_NO_PYTHON Python3::NumPy)
    set(DEPS_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS} ${Python3_INCLUDE_DIRS} ${Python3_NumPy_INCLUDE_DIRS})
endif()

##################
# IPC dependency #
##################

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
    IF (WIN32)
        set(CONDA_INCLUDE ${CONDA_PREFIX}/Library/include)
    else()
        set(CONDA_INCLUDE ${CONDA_PREFIX}/include)
    endif()
    # set(DEPS_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS} ${CONDA_INCLUDE})
endif(CONDA_PREFIX)

if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
   set(CMAKE_CXX_FLAGS_DEBUG "/Zi -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELEASE "/Zi -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/Zi -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
else ()
   set(CMAKE_CXX_FLAGS_DEBUG "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} ")
   set(CMAKE_CXX_FLAGS_RELEASE "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} ")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g -DNDEBUG -DYGG_DEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} ")
endif ()
# if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
#     set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -D_GLIBCXX_DEBUG")
# endif()



set(DEPS_INCLUDE_DIRS ${DEPS_INCLUDE_DIRS} ${CMAKE_SOURCE_DIR}/communication)
message(STATUS "DEPS_INCLUDE_DIRS = ${DEPS_INCLUDE_DIRS}")
message(STATUS "DEPS_LIBRARIES = ${DEPS_LIB_NAMES};${DEPS_LIBRARIES}")

####################
# Create library   #
####################

if(SKBUILD)
  set(BUILD_CPP_LIBRARY ON)
endif()
if(BUILD_CPP_LIBRARY)
    if(SKBUILD)
        set(YGG_TARGET_CPP YggInterface_py)
    else()
        set(YGG_TARGET_CPP YggInterface)
    endif()
    message(STATUS "YGG_TARGET_CPP = ${YGG_TARGET_CPP}")
    file(GLOB SOURCES "communication/*.cpp" "communication/utils/*.cpp" "communication/communicators/*.cpp" "communication/datatypes/dtype_t.cpp")
    if(SKBUILD AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
        message(STATUS "SKBUILD_PROJECT_NAME = ${SKBUILD_PROJECT_NAME}")
	message(STATUS "SKBUILD_PLATLIB_DIR = ${SKBUILD_PLATLIB_DIR}")
	if(SKBUILD STREQUAL "2")
  	    Python3_add_library(${YGG_TARGET_CPP} SHARED ${SOURCES})
	    set(YGG_CPP_LINK_KEYWORD PUBLIC)
	else()
            add_library(${YGG_TARGET_CPP} SHARED ${SOURCES})
	endif()
        target_link_libraries(
	    ${YGG_TARGET_CPP} ${YGG_CPP_LINK_KEYWORD}
	    ${DEPS_LIB_NAMES_NO_PYTHON}
	    ${DEPS_LIBRARIES_NO_PYTHON})
     	target_compile_options(${YGG_TARGET_CPP} PUBLIC
     	    -DRAPIDJSON_YGGDRASIL_PYTHON)
    else()
        add_library(${YGG_TARGET_CPP} SHARED ${SOURCES})
        target_link_libraries(
	    ${YGG_TARGET_CPP} PUBLIC
	    ${DEPS_LIB_NAMES}
	    ${DEPS_LIBRARIES})
	if (WIN32)
	    set_target_properties(${YGG_TARGET_CPP}
	        PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
	endif()
    endif()
    target_include_directories(
        ${YGG_TARGET_CPP} PUBLIC
    	${DEPS_INCLUDE_DIRS})
endif()

#########################
# Create Python Library #
#########################

if(SKBUILD)
    set(BUILD_PYTHON_LIBRARY ON)
endif(SKBUILD)
if(BUILD_PYTHON_LIBRARY AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
    add_subdirectory(python)
endif()

#########################
# Create Fortran Library #
#########################

if(BUILD_FORTRAN_LIBRARY)
    enable_language(Fortran)
    add_subdirectory(fortran)
endif()

##################
# Coverage       #
##################

if(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
    message(STATUS "COVERAGE ON")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 --coverage -fno-inline -fprofile-arcs -ftest-coverage -fprofile-filter-files=\"${CMAKE_SOURCE_DIR}/communication/*\" ")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0 --coverage -fno-inline -fprofile-arcs -ftest-coverage -fprofile-filter-files=\"${CMAKE_SOURCE_DIR}/communication/*\" ")
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
        set(CTEST_COVERAGE_COMMAND "llvm-cov")
        set(CTEST_COVERAGE_EXTRA_FLAGS "gcov")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
    else()
        target_link_libraries(YggInterface PRIVATE gcov)
        set(CTEST_COVERAGE_COMMAND "gcov")
        set(CTEST_COVERAGE_EXTRA_FLAGS "")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-inline-small-functions -fno-default-inline")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-inline-small-functions -fno-default-inline")
    endif()
    # find required tools
    # find_program(LCOV lcov REQUIRED)
    # find_program(GENHTML genhtml REQUIRED)
    # add_custom_target(coverage
    #         # gather data
    #         COMMAND ${LCOV} --rc lcov_branch_coverage=1 --exclude '/usr/*' --exclude '/opt/*' --directory . --capture --output-file coverage.info
    #         # generate report
    #         COMMAND ${GENHTML} --rc genhtml_branch_coverage=1 --demangle-cpp -o coverage coverage.info
    #         WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
    # set(YGG_BUILD_FLAGS "${YGG_BUILD_FLAGS} --coverage -fno-inline -fno-inline-small-functions -fno-default-inline ")
endif(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")

#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS OR YGG_BUILD_FORTRAN_TESTS)
    include(CTest)
    add_definitions(-DYGG_TEST)
endif()
if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS)
    message("TESTING ON")
    set(SUBLIBFILE ${CMAKE_BINARY_DIR}/library_path.txt)
    add_custom_command(
        OUTPUT ${SUBLIBFILE}
        COMMAND echo "$<TARGET_FILE:YggInterface>" > "${SUBLIBFILE}"
       DEPENDS YggInterface
       VERBATIM)
    add_custom_target(make_sublib ALL DEPENDS ${SUBLIBFILE})

    if(MSVC11)
        # required for VS2012 due to missing support for variadic templates
        add_definitions(-D_VARIADIC_MAX=10)
    endif(MSVC11)
    add_subdirectory(test)

endif()

