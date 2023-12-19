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
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

if (WIN32)
   set(CMAKE_CXX_EXTENSIONS OFF)
endif()
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(BUILD_CPP_LIBRARY "Build the Yggdrasil C++ interface library" ON)
option(BUILD_FORTRAN_LIBRARY "Build the Yggdrasil Fortran interface library" ON)
option(BUILD_PYTHON_LIBRARY "Build the Yggdrasil Python interface library" OFF)
option(YGG_PYTHON_LIBRARY_WRAP "Build the Yggdrasil Python interface library by wrapping the C++ module" ON)
option(YGG_Fortran_REQUIRED "Raise an error if the Fortran interface library cannot be build" OFF)
option(ENABLE_ZMQ "Enable ZeroMQ" ON)
option(ENABLE_RMQ "Enable RabbitMQ" ON)
option(USING_IPC "Default to using IPC V queues" OFF)
option(YGGDRASIL_DISABLE_PYTHON_C_API "Disable the Python C API" OFF)
option(ENABLE_OMP "Enable OpenMP" ON)
option(ENABLE_MPI "Enable MPI" ON)
option(ENABLE_THREADS "Enable std::threads" ON)
option(YGG_BUILD_TESTS "Build tests" OFF)
option(YGG_BUILD_FORTRAN_TESTS "Build Fortran tests" OFF)
option(YGG_BUILD_CXX_TESTS "Build C/C++ tests" OFF)
option(YGG_BUILD_PYTHON_TESTS "Build Python tests" OFF)
# option(YGG_TEST_WITH_LLDB "Run tests with lldb" OFF)
option(YGG_ENABLE_COVERAGE "Enable coverage reporting" OFF)
option(YGG_BUILD_THIRDPARTY_GTEST "Use gtest installation in 'thirdpart/gtest' by default if available" ON)
option(YGG_SKIP_VALGRIND_TESTS "Disable valgrind tests" OFF)
option(YGG_BUILD_ASAN "Build with address sanitizer (gcc/clang)" OFF)
option(YGG_BUILD_UBSAN "Build with undefined behavior sanitizer (gcc/clang)" OFF)
option(YGG_ENABLE_INSTRUMENTATION_OPT "Build yggdrasil with -march or -mcpu options" ON)
option(WRAP_RAPIDJSON_FOR_DLL "Build yggdrasil using a wrapper for rapidjson to force it into a DLL (enabled automatically when compiling with MSVC" OFF)
option(YGG_LINK_PYTHON_TO_CPP "Link the Python extension to the C++ wrapper as a library" OFF)
option(YGG_DEBUG_LEVEL "Level that should be used for logging" OFF)

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
	    list(APPEND ASAN_COMPILE_FLAGS -fsanitize=address)
	    list(APPEND ASAN_LINK_FLAGS -fsanitize=address)
        endif()
    endif()
    if (YGG_BUILD_UBSAN)
        if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.9.0")
            message(FATAL_ERROR "GCC < 4.9 doesn't support the undefined behavior sanitizer")
        else()
	    list(APPEND ASAN_COMPILE_FLAGS -fsanitize=undefined)
	    list(APPEND ASAN_LINK_FLAGS -fsanitize=undefined)
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
	list(APPEND ASAN_COMPILE_FLAGS -fsanitize=address)
	list(APPEND ASAN_LINK_FLAGS -fsanitize=address)
    endif()
    if (YGG_BUILD_UBSAN)
        if (CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
	    list(APPEND ASAN_COMPILE_FLAGS -fsanitize=undefined-trap -fsanitize-undefined-trap-on-error)
	    list(APPEND ASAN_LINK_FLAGS -fsanitize=undefined-trap -fsanitize-undefined-trap-on-error)
        else()
	    list(APPEND ASAN_COMPILE_FLAGS -fsanitize=undefined)
	    list(APPEND ASAN_LINK_FLAGS -fsanitize=undefined)
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
list(APPEND DEPS_PUBLIC_C_COMPILE_FLAGS ${ASAN_COMPILE_FLAGS})
list(APPEND DEPS_PUBLIC_CXX_COMPILE_FLAGS ${ASAN_COMPILE_FLAGS})
list(APPEND DEPS_PUBLIC_C_LINK_FLAGS ${ASAN_LINK_FLAGS})
list(APPEND DEPS_PUBLIC_CXX_LINK_FLAGS ${ASAN_LINK_FLAGS})

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

if(WIN32)
  if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(WRAP_RAPIDJSON_FOR_DLL ON)
  # else()
  #     set(YGG_LINK_PYTHON_TO_CPP ON)
  endif()
endif()

if(WRAP_RAPIDJSON_FOR_DLL)
    set(YGG_LINK_PYTHON_TO_CPP OFF)
    list(APPEND YGG_INSTALL_DEFS -DWRAP_RAPIDJSON_FOR_DLL)
endif()

list(APPEND YGG_INSTALL_DEFS
     -DRAPIDJSON_YGGDRASIL -DRAPIDJSON_HAS_STDSTRING
     -DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION
     -D_USE_MATH_DEFINES)

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
    set(RAPIDJSON_INCLUDE_DIRS "${CMAKE_SOURCE_DIR}/src/rapidjson/include")
endif()
message(STATUS "Rapidjson found ${RAPIDJSON_INCLUDE_DIRS}")

##############################

##########
# ZeroMQ #
##########

if(ENABLE_ZMQ)
    include(SearchTools)
    find_package_zmq()
    if (ZeroMQ_FOUND)
        list(APPEND YGG_INSTALL_DEFS -DZMQINSTALLED)
        list(APPEND YGG_INSTALL_DEPS ZeroMQ)
	list(APPEND DEPS_LIBRARIES ${ZeroMQ_LIBRARY})
	list(APPEND DEPS_INCLUDE_DIRS ${ZeroMQ_INCLUDE_DIR})
	execute_process(
	  COMMAND ls ${ZeroMQ_INCLUDE_DIR}
	  COMMAND_ECHO STDOUT)
    else()
        message(STATUS "NO ZeroMQ")
    endif()
else (ENABLE_ZMQ)
    message(STATUS "No zmq support will be built." )
endif (ENABLE_ZMQ)

############
# RabbitMQ #
############

if(ENABLE_RMQ)
    find_package_pkgconfig(rabbitmq-c HEADER amqp.h LIBNAMES rabbitmq)
    if (rabbitmq-c_FOUND)
        include(${rabbitmq-c_CONFIG})
        list(APPEND YGG_INSTALL_DEFS -DRMQINSTALLED)
        list(APPEND YGG_INSTALL_DEPS rabbitmq-c)
	list(APPEND DEPS_LIBRARIES rabbitmq::rabbitmq)
	if (rabbitmq-c_DIR)
	  execute_process(
	    COMMAND ls ${rabbitmq-c_DIR}
	    COMMAND_ECHO STDOUT)
	endif()
	# In v0.12.0 (v9.2.5 so), the include files were moved into a
	# dedicated rabbitmq-c directory
	if (((rabbitmq-c_DIR MATCHES "*vcpkg*") AND
	     (rabbitmq-c_VERSION VERSION_LESS "9.2.5")) OR
	    (rabbitmq-c_VERSION VERSION_LESS "0.12.0"))
          list(APPEND DEPS_PUBLIC_C_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
	  list(APPEND DEPS_PUBLIC_CXX_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
	endif()
    else()
        message(STATUS "NO RabbitMQ client library")
    endif()
    dump_cmake_variables(REGEX "rabbitmq*")
else (ENABLE_RMQ)
    message(STATUS "No RabbitMQ support will be built." )
endif (ENABLE_RMQ)

##########
# OpenMP #
##########

if (ENABLE_OMP)
  find_package(OpenMP)
  if (OpenMP_CXX_FOUND)
    message(STATUS "OpenMP found")
    list(APPEND YGG_INSTALL_DEPS OpenMP)
    list(APPEND DEPS_LIBRARIES OpenMP::OpenMP_CXX)
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
  cmake_path(SET MINGW_PTHREAD_ROOT "$ENV{MINGW_PTHREAD_ROOT}")
  if(MINGW_PTHREAD_ROOT)
    message(STATUS "MINGW_PTHREAD_ROOT = ${MINGW_PTHREAD_ROOT}")
    find_package(Threads PATHS ${MINGW_PTHREAD_ROOT} NO_DEFAULT_PATH)
  endif()
  find_package(Threads)
  if (Threads_FOUND)
    list(APPEND YGG_INSTALL_DEFS -DTHREADSINSTALLED)
    if (CMAKE_THREAD_LIBS_INIT)
      message(STATUS "CMAKE_THREAD_LIBS_INIT = ${CMAKE_THREAD_LIBS_INIT}")
      list(APPEND DEPS_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})
    endif()
    if (CMAKE_USE_PTHREADS_INIT)
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
    endif()
    list(APPEND YGG_INSTALL_DEPS Threads)
    list(APPEND DEPS_LIBRARIES Threads::Threads)
  else()
    message(STATUS "NO Threads")
  endif()
endif()

#############################
# Patch MinGW 4.3.5 Threads #
#############################

# The version of MinGW provided by conda-forge (5.3.0) does not contain
# the necessary comparison operators for the _M_thread members of
# thread::id. This patch comments out those operators and modifies the
# hash to include both the p & x members of _M_thread.

if (CONDA_PREFIX AND MINGW AND
    CMAKE_CXX_COMPILER_VERSION STREQUAL "5.3.0")
  cmake_path(APPEND CONDA_PREFIX Library mingw-w64 include c++ 5.3.0
             OUTPUT_VARIABLE MINGW_PATHS)
  message(STATUS "MINGW_PATHS = ${MINGW_PATHS}")
  find_file(THREAD_H thread PATHS ${MINGW_PATHS} NO_DEFAULT_PATH REQUIRED)
  message(STATUS "THREAD_H = ${THREAD_H}")
  if(NOT THREAD_H STREQUAL THREAD_H-NOTFOUND)
    file(READ ${THREAD_H} THREAD_H_CONTENTS)
    if(THREAD_H_CONTENTS MATCHES "^// mingw\\-c\\+\\+\\-5\\-3\\-thread\\.patch applied.*")
      message(STATUS "MinGW 5.3.0 thread already patched")
    else()
      message(STATUS "Patching MinGW 5.3.0 thread")
      cmake_path(APPEND CMAKE_SOURCE_DIR "mingw-c++-5-3-thread.patch"
                 OUTPUT_VARIABLE THREAD_H_PATCH)
      message(STATUS "THREAD_H_PATCH = ${THREAD_H_PATCH}")
      if (NOT EXISTS ${THREAD_H_PATCH})
        message(FATAL_ERROR "THREAD_H_PATCH does not exist (${THREAD_H_PATCH})")
      endif()
      execute_process(
        COMMAND patch -b ${THREAD_H} ${THREAD_H_PATCH}
	TIMEOUT 15
	COMMAND_ECHO STDOUT
	RESULT_VARIABLE ret)
      if (NOT ret EQUAL 0)
        message(FATAL_ERROR "Failed to apply MinGW thread patch")
      endif()
    endif()
  endif()
endif()

#######
# MPI #
#######

if(ENABLE_MPI)
  find_package(MPI)
  if(MPI_CXX_FOUND)
    message(STATUS "MPI found")
    list(APPEND YGG_INSTALL_DEFS -DMPIINSTALLED)
    list(APPEND YGG_INSTALL_DEPS MPI)
    list(APPEND DEPS_LIBRARIES MPI::MPI_CXX)
  else()
    message(STATUS "NO MPI")
  endif()
endif()

##########
# Python #
##########

if(YGGDRASIL_DISABLE_PYTHON_C_API)
    list(APPEND YGG_INSTALL_DEFS -DYGGDRASIL_DISABLE_PYTHON_C_API)
else()
    include(SearchTools)
    find_package_python()

    # There is an issue with embedding Python on Windows in conda environments
    # https://github.com/ContinuumIO/anaconda-issues/issues/11374
    if(WIN32)
        set(RAPIDJSON_PYTHON_ENV2 "PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
    endif()
    
    list(APPEND YGG_INSTALL_DEPS Python3)
    list(APPEND DEPS_LIBRARIES Python3::NumPy)
    list(APPEND DEPS_LIBRARIES_PYTHON Python3::Python)
endif()

##################
# IPC dependency #
##################

if (NOT ZeroMQ_FOUND)
    set(USING_IPC ON)
endif()
if(USING_IPC)
    if (UNIX OR CYGWIN)
	list(APPEND YGG_INSTALL_DEFS -DIPCDEF)
    else()
        set(USING_IPC OFF)
    endif()
endif()
if (UNIX OR CYGWIN)
    list(APPEND YGG_INSTALL_DEFS -DIPCINSTALLED)
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
endif(CONDA_PREFIX)

if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
   set(CMAKE_CXX_FLAGS_DEBUG "/Zi -DNDEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELEASE "/Zi -DNDEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/Zi -DNDEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS}")
else ()
   set(CMAKE_CXX_FLAGS_DEBUG "-g3 -ggdb -DNDEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} ")
   set(CMAKE_CXX_FLAGS_RELEASE "-g -DNDEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} ")
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-g -DNDEBUG${BUILD_FLAGS}${YGG_BUILD_FLAGS} ")
endif ()
# if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
#     set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -D_GLIBCXX_DEBUG")
# endif()
if (YGG_DEBUG_LEVEL)
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DYGG_DEBUG=${YGG_DEBUG_LEVEL}")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -DYGG_DEBUG=${YGG_DEBUG_LEVEL}")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -DYGG_DEBUG=${YGG_DEBUG_LEVEL}")
endif()

message(STATUS "DEPS_INCLUDE_DIRS = ${DEPS_INCLUDE_DIRS}")
message(STATUS "DEPS_LIBRARIES = ${DEPS_LIBRARIES}")

####################
# Create library   #
####################

if(SKBUILD)
  if(YGG_LINK_PYTHON_TO_CPP)
    set(BUILD_CPP_LIBRARY ON)
  else()
    set(BUILD_CPP_LIBRARY OFF)
    set(BUILD_FORTRAN_LIBRARY OFF)
  endif()
elseif(BUILD_PYTHON_LIBRARY AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
  set(BUILD_CPP_LIBRARY ON)
endif()
if(BUILD_FORTRAN_LIBRARY)
  set(BUILD_CPP_LIBRARY ON)
endif()
file(GLOB SOURCES "src/*.cpp" "src/utils/*.cpp" "src/communicators/*.cpp" "src/datatypes/dtype_t.cpp")

if(BUILD_CPP_LIBRARY)
    if(SKBUILD)
        set(YGG_TARGET_CPP YggInterface_py)
    else()
        set(YGG_TARGET_CPP YggInterface)
    endif()
    if(SKBUILD AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
	if(SKBUILD STREQUAL "2")
  	    Python3_add_library(${YGG_TARGET_CPP} SHARED ${SOURCES})
	    set(YGG_CPP_LINK_KEYWORD PUBLIC)
	else()
            add_library(${YGG_TARGET_CPP} SHARED ${SOURCES})
	endif()
     	target_compile_options(${YGG_TARGET_CPP} PUBLIC
     	    -DRAPIDJSON_YGGDRASIL_PYTHON -DYGG_LINK_PYTHON_TO_CPP)
    else()
        add_library(${YGG_TARGET_CPP} SHARED ${SOURCES})
	add_library(YggInterface::CXX ALIAS ${YGG_TARGET_CPP})
	add_library(YggInterface::C ALIAS ${YGG_TARGET_CPP})
        target_link_libraries(
	    ${YGG_TARGET_CPP} PUBLIC
	    ${DEPS_LIBRARIES_PYTHON})
	if (WIN32)
	    set_target_properties(${YGG_TARGET_CPP}
	        PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
	endif()
	if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
	    target_link_options(${YGG_TARGET_CPP}
	        PRIVATE /INCLUDE:rapidjson_ARRAY_API)
	endif()
        list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_CPP})
    endif()
    target_link_libraries(
        ${YGG_TARGET_CPP} PRIVATE ${DEPS_LIBRARIES_PRIVATE})
    target_link_libraries(
        ${YGG_TARGET_CPP} PUBLIC ${DEPS_LIBRARIES}
	"$<$<LINK_LANGUAGE:C>:${DEPS_PUBLIC_C_LIBS}>"
	"$<$<LINK_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_LIBS}>"
	"$<$<LINK_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_LIBS}>")
    target_include_directories(
        ${YGG_TARGET_CPP} PUBLIC ${DEPS_INCLUDE_DIRS}
  	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
	$<BUILD_INTERFACE:${RAPIDJSON_INCLUDE_DIRS}>
	$<INSTALL_INTERFACE:include/YggInterface>
	$<INSTALL_INTERFACE:include/YggInterface/rapidjson/include>)
    # target_include_directories(
    #     ${YGG_TARGET_CPP} PRIVATE ${DEPS_INCLUDE_DIRS})
    target_compile_definitions(
        ${YGG_TARGET_CPP} PUBLIC ${YGG_INSTALL_DEFS})
    target_compile_options(
	${YGG_TARGET_CPP} PUBLIC ${DEPS_PUBLIC_COMPILE_FLAGS}
	"$<$<COMPILE_LANGUAGE:C>:${DEPS_PUBLIC_C_COMPILE_FLAGS}>"
	"$<$<COMPILE_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_COMPILE_FLAGS}>"
	"$<$<COMPILE_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_COMPILE_FLAGS}>")
    target_link_options(
        ${YGG_TARGET_CPP} PUBLIC ${DEPS_PUBLIC_LINK_FLAGS}
	"$<$<LINK_LANGUAGE:C>:${DEPS_PUBLIC_C_LINK_FLAGS}>"
	"$<$<LINK_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_LINK_FLAGS}>"
	"$<$<LINK_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_LINK_FLAGS}>")
    target_compile_features(
        ${YGG_TARGET_CPP} PUBLIC cxx_std_14)
    
    include(CheckDLL)
    show_runtimes(${YGG_TARGET_CPP})
endif()

#########################
# Create Python Library #
#########################

if(SKBUILD)
    set(BUILD_PYTHON_LIBRARY ON)
    add_subdirectory(python)
elseif(BUILD_PYTHON_LIBRARY AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
    set(PYTHON_COPY_ARGS ENABLE_OMP USING_IPC ENABLE_OMP ENABLE_MPI
        ENABLE_THREADS YGG_ENABLE_COVERAGE
	YGG_SKIP_VALGRIND_TESTS YGG_BUILD_ASAN YGG_BUILD_UBSAN
	YGG_ENABLE_INSTRUMENTATION_OPT WRAP_RAPIDJSON_FOR_DLL
	YGG_DEBUG_LEVEL)
    foreach(opt IN LISTS PYTHON_COPY_ARGS)
      list(APPEND PYTHON_CMAKE_ARGS "-D${opt}=${${opt}}")
    endforeach()
    if (CMAKE_PREFIX_PATH)
      list(APPEND PYTHON_BUILD_ARGS
        --config-settings=cmake.define.CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH})
    endif()
    if (Python3_ROOT_DIR)
      list(APPEND PYTHON_BUILD_ARGS
        --config-settings=cmake.define.Python3_ROOT_DIR=${Python3_ROOT_DIR}
        --config-settings=cmake.define.Python_ROOT_DIR=${Python3_ROOT_DIR})
    endif()
    message(STATUS "Building Python with CMAKE_ARGS = ${PYTHON_CMAKE_ARGS}")
    
    set(OLD_CMAKE_ARGS "$ENV{CMAKE_ARGS}")
    set(ENV{CMAKE_ARGS} "${PYTHON_CMAKE_ARGS}")
    execute_process(
      COMMAND ${Python3_EXECUTABLE} -m pip install ${CMAKE_CURRENT_SOURCE_DIR} -v ${PYTHON_BUILD_ARGS}
      COMMAND_ECHO STDOUT
      RESULT_VARIABLE ret)
    message(STATUS "Resetting CMAKE_ARGS = ${OLD_CMAKE_ARGS}")
    set(ENV{CMAKE_ARGS} "${OLD_CMAKE_ARGS}")
    if (NOT ret EQUAL 0)
      message(FATAL_ERROR "Failed to build Python via pip")
    endif()
    if(YGG_BUILD_TESTS OR YGG_BUILD_PYTHON_TESTS)
      add_subdirectory(python/test)
    endif()
endif(SKBUILD)

##########################
# Create Fortran Library #
##########################

if(BUILD_FORTRAN_LIBRARY)
    if(CMAKE_Fortran_COMPILER AND WIN32)
        cmake_path(SET CMAKE_Fortran_COMPILER NORMALIZE ${CMAKE_Fortran_COMPILER})
    endif()
    add_subdirectory(fortran)
endif()

##################
# Install        #
##################

if (YGG_INSTALL_TARGETS)
  include(GNUInstallDirs)
  install(TARGETS ${YGG_INSTALL_TARGETS}
          EXPORT YggInterfaceTargets
	  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
	  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
	  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
	  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/YggInterface)
  install(EXPORT YggInterfaceTargets
          FILE YggInterfaceTargets.cmake
	  NAMESPACE YggInterface::
	  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/YggInterface)
  install(DIRECTORY src/
          DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/YggInterface
	  FILES_MATCHING REGEX ".*\\.h(pp)?$")
  include(CMakePackageConfigHelpers)
  write_basic_package_version_file(
    "YggInterfaceConfigVersion.cmake"
    VERSION ${LIB_VERSION_STRING}
    COMPATIBILITY AnyNewerVersion)
  configure_file(
    YggInterfaceConfig.cmake.in YggInterfaceConfig.cmake @ONLY)
  file(GLOB_RECURSE modules_raw LIST_DIRECTORIES false
       "${CMAKE_CURRENT_LIST_DIR}/cmake/*.cmake")
  foreach(f IN LISTS modules_raw)
    if (NOT f MATCHES "Find*")
      list(APPEND modules ${f})
    endif()
  endforeach()
  install(DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/cmake/CMakeAddFortranSubdirectory
          DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/YggInterface
          FILES_MATCHING REGEX ".*\\.in$")
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/YggInterfaceConfig.cmake"
          "${CMAKE_CURRENT_BINARY_DIR}/YggInterfaceConfigVersion.cmake"
	  ${modules}
          DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/YggInterface)
  if (YGG_INSTALL_INCLUDES)
    install(FILES ${YGG_INSTALL_INCLUDES}
            DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/YggInterface)
  endif()
endif ()
    
##################
# Coverage       #
##################

if(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
    message(STATUS "COVERAGE ON")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 --coverage -fno-inline -fprofile-arcs -ftest-coverage -fprofile-filter-files=\"${CMAKE_SOURCE_DIR}/src/*\" ")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0 --coverage -fno-inline -fprofile-arcs -ftest-coverage -fprofile-filter-files=\"${CMAKE_SOURCE_DIR}/src/*\" ")
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
        set(CTEST_COVERAGE_COMMAND "llvm-cov")
        set(CTEST_COVERAGE_EXTRA_FLAGS "gcov")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
    else()
        target_link_libraries(${YGG_TARGET_CPP} PRIVATE gcov)
        set(CTEST_COVERAGE_COMMAND "gcov")
        set(CTEST_COVERAGE_EXTRA_FLAGS "")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-inline-small-functions -fno-default-inline")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-inline-small-functions -fno-default-inline")
    endif()
endif(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")

#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS OR YGG_BUILD_FORTRAN_TESTS OR YGG_BUILD_PYTHON_TESTS)
    enable_testing()
    include(CTest)
endif()
if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS)
    message(STATUS "TESTING ON")
    set(SUBLIBFILE ${CMAKE_BINARY_DIR}/library_path.txt)
    add_custom_command(
        OUTPUT ${SUBLIBFILE}
        COMMAND echo "$<TARGET_FILE:${YGG_TARGET_CPP}>" > "${SUBLIBFILE}"
       DEPENDS ${YGG_TARGET_CPP}
       VERBATIM)
    add_custom_target(make_sublib ALL DEPENDS ${SUBLIBFILE})

    if(MSVC11)
        # required for VS2012 due to missing support for variadic templates
        add_definitions(-D_VARIADIC_MAX=10)
    endif(MSVC11)
    add_subdirectory(test)

endif()

