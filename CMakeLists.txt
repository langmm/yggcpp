# Options to make build super verbose
# -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON
# -DCMAKE_MESSAGE_LOG_LEVEL:STRING=DEBUG
cmake_minimum_required(VERSION 3.18)
if(POLICY CMP0048)
  # Project manages VERSION, OLD behavior removed in 4.0
  cmake_policy(SET CMP0048 NEW)
endif()
if(POLICY CMP0135)
  # ExternalProject_Add set timestamp of extracted files to the
  # time at extraction
  cmake_policy(SET CMP0135 NEW)
endif()
if(POLICY CMP0094)  # https://cmake.org/cmake/help/latest/policy/CMP0094.html
  cmake_policy(SET CMP0094 NEW)  # FindPython should return the first matching Python
endif()
if(POLICY CMP0057)  # IN_LIST operator
  cmake_policy(SET CMP0057 NEW)
endif()
if(SKBUILD)
  set(YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER ON)
endif()

message(DEBUG "TOP LEVEL CMAKE_GENERATOR=${CMAKE_GENERATOR}")
message(DEBUG "CMAKE_PREFIX_PATH = ${CMAKE_PREFIX_PATH}")

SET(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules")
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(YggInterfaceMacros)

ygginterface_gitversion(DEFAULT_YGGINTERFACE_VERSION "0.1.0")
set(YGGINTERFACE_VERSION "${DEFAULT_YGGINTERFACE_VERSION}" CACHE STRING "Version to use instead of the git tag version")
message(STATUS "YGGINTERFACE_VERSION = ${YGGINTERFACE_VERSION}")

# compile in release with debug info mode by default
if(NOT CMAKE_BUILD_TYPE)
  # This must be before the project call or the default Debug value used
  # by the Ninja generator will be used
  # https://discourse.cmake.org/t/how-to-deal-with-ninja-setting-cmake-build-type-to-debug/281
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

PROJECT(YggInterface VERSION "${YGGINTERFACE_VERSION}")

if (SKBUILD)
  message(STATUS "SKBUILD_PROJECT_NAME = ${SKBUILD_PROJECT_NAME}")
  message(STATUS "SKBUILD_PROJECT_VERSION = ${SKBUILD_PROJECT_VERSION}")
  if("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
    message(FATAL_ERROR "Building Python package with debug symbols disabled")
  endif()
  if (WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
  endif()
endif()

if (APPLE AND "$ENV{SDKROOT}")
  SET(CMAKE_OSX_SYSROOT "$ENV{SDKROOT}")
endif()
if (UNIX)
  include(GNUInstallDirs)
endif()
if (WIN32)
   set(CMAKE_CXX_EXTENSIONS OFF)
endif()

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

ygginterface_options_create()

include(AddYggInterface)
include(BuildTools)

option(YGG_Python_LIBRARY_WRAP "Build the Yggdrasil Python interface library by wrapping the C++ module" ON)
option(ALLOW_SKBUILD_NONPython "Allow the non-Python interfaces (C/C++/Fortran) to be built from SKBUILD" OFF)
option(YGG_BUILD_TESTS "Build tests" OFF)
# option(YGG_TEST_WITH_LLDB "Run tests with lldb" OFF)
option(YGG_BUILD_THIRDPARTY_GTEST "Use gtest installation in 'thirdpart/gtest' by default if available" ON)
option(YGG_SKIP_VALGRIND_TESTS "Disable valgrind tests" OFF)
option(GENERATE_YGGDRASIL_RAPIDJSON_WRAPPER "Generate the yggdrasil_rapidjson wrapper from the C++ interface" OFF)
option(GENERATE_INTERFACE "Generate the interface" OFF)
option(YGG_BUILD_DOCS "Build documentation (requires Doxygen)" OFF)
option(DOXYGEN_CHECK_MISSING "Raise an error if documentation is missing" OFF)
option(YGG_PATCH_MINGW53_THREAD "Patch the MinGW 5.3.0 thread header" OFF)
option(YGG_Fortran_WRAP_C_ENUMS "Wrap the int64_t enumerators in a C layer for binding in Fortran" OFF)
option(YGG_INSPECT_TARGETS "Inspect the executables and libraries that are built" OFF)
option(YGG_INSPECT_TARGETS_ONLY_MISSING "Show library links that cannot be resolved" OFF)
if(YGG_INSPECT_TARGETS_ONLY_MISSING)
  set(YGG_INSPECT_TARGETS ON)
endif()

# Set this early so they can be used in code generation
set(YGG_TARGET_CXX YggInterface)
set(YGG_TARGET_Fortran YggInterface_fortran)
# set(YGG_TARGET_Fortran YggInterfaceFortran)
var_dependency(YGG_TARGET_Fortran_NAME YGG_TARGET_Fortran)

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
    if (WIN32)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wa,-mbig-obj")
    endif()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror")
    set(EXTRA_CXX_FLAGS -Weffc++ -Wswitch-default -Wfloat-equal -Wconversion -Wsign-conversion)
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
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    add_definitions(-D_CRT_SECURE_NO_WARNINGS=1)
    add_definitions(-DNOMINMAX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc /bigobj /Zm10")
    # Always compile with /WX
    if(CMAKE_CXX_FLAGS MATCHES "/WX-")
        string(REGEX REPLACE "/WX-" "/WX" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /WX")
    endif()
elseif (CMAKE_CXX_COMPILER_ID MATCHES "XL")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -qarch=auto")
endif()

ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS)

##################################
# Option dependent configuration #
##################################

ygginterface_options_config_init(DEPS)
set(DEPS_Python_LIBRARIES)
ygginterface_options_config(DEPS)

include(FetchContent)

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
    message(FATAL_ERROR "In-source build prohibited.")
endif("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

list(APPEND YGG_TEST_ENV "YGG_MODEL_INDEX=123")

set(YGG_BUILD_FLAGS "")


#################
# Documentation #
#################

if(YGG_BUILD_DOCS)
    find_package(Doxygen)
    if(NOT Doxygen_FOUND)
        message(WARNING "Doxygen not found, documentation will not be produced.")
        set(YGG_BUILD_DOCS OFF CACHE BOOL "")
    else()
        set(DOXYGEN_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/doc CACHE INTERNAL "")
        set(DOXYGEN_INPUT_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "")
	if (DOXYGEN_CHECK_MISSING)
	    set(DOXYGEN_WARN_AS_ERROR YES)
	    set(DOXYGEN_EXTRACT_ALL NO)
	else()
	    set(DOXYGEN_WARN_AS_ERROR NO)
	    set(DOXYGEN_EXTRACT_ALL YES)
	endif()
        file(MAKE_DIRECTORY ${DOXYGEN_BUILD_DIR})
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
        add_custom_target(docs
                          COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
                          WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                          VERBATIM)
    endif()
endif()

#########################################
# DEPENDENCIES
#########################################

cmake_path(SET CONDA_PREFIX "$ENV{CONDA_PREFIX}")
if(CONDA_PREFIX)
    message(DEBUG "CONDA_PREFIX = ${CONDA_PREFIX}")
endif()

######################
# YggdrasilRapidJSON #
######################

message(DEBUG "YggdrasilRapidJSON_DIR = ${YggdrasilRapidJSON_DIR}")
add_yggdrasil_dependency(
  YggdrasilRapidJSON
  LIBRARIES YggdrasilRapidJSON
  REQUIRED
)
if(SKBUILD)
  if(YggdrasilRapidJSON_VERSION VERSION_LESS "1.1.0.6")
    strip_python(YggdrasilRapidJSON)
  endif()
  get_target_property(
    YggdrasilRapidJSON_LINK_LIBRARIES
    YggdrasilRapidJSON INTERFACE_LINK_LIBRARIES
  )
  list(FILTER YggdrasilRapidJSON_LINK_LIBRARIES INCLUDE REGEX ".+Python$")
  if(YggdrasilRapidJSON_LINK_LIBRARIES)
    message(FATAL_ERROR "YggdrasilRapidJSON_LINK_LIBRARIES = ${YggdrasilRapidJSON_LINK_LIBRARIES}")
  endif()
endif()

##########
# ZeroMQ #
##########

add_yggdrasil_option(ZMQ COMM DEPENDENCY ZeroMQ PRIVATE)

##################
# IPC dependency #
##################

add_yggdrasil_option(
  IPC COMM NO_DEPENDENCY
  SUPPORTED_OS UNIX CYGWIN
)

############
# RabbitMQ #
############

add_yggdrasil_option(RMQ COMM DEPENDENCY rabbitmq-c PRIVATE)
if (rabbitmq-c_FOUND)
  # In v0.12.0 (v9.2.5 so), the include files were moved into a
  # dedicated rabbitmq-c directory. This is complicated by the
  # so version (and cmake version) not matching the source version.
  if (rabbitmq-c_VERSION)
    message(DEBUG "rabbitmq-c_VERSION = ${rabbitmq-c_VERSION}")
    if (rabbitmq-c_DIR MATCHES ".*vcpkg.*")
      message(DEBUG "rabbitmq-c_DIR MATCHES *vcpkg*")
    else()
      message(DEBUG "rabbitmq-c_DIR DOES NOT MATCH *vcpkg*")
    endif()
    if (rabbitmq-c_VERSION VERSION_LESS "0.12.0")
      message(DEBUG "rabbitmq-c_VERSION LESS 0.12.0")
    else()
      message(DEBUG "rabbitmq-c_VERSION MORE 0.12.0")
    endif()
    # if (rabbitmq-c_VERSION VERSION_LESS "9.2.5")
    #   message(DEBUG "rabbitmq-c_VERSION LESS 9.2.5")
    # else()
    #   message(DEBUG "rabbitmq-c_VERSION MORE 9.2.5")
    # endif()
  else()
    message(DEBUG "rabbitmq-c_VERSION NOT SET")
  endif()
  if ((NOT rabbitmq-c_VERSION) OR
      (rabbitmq-c_VERSION VERSION_GREATER "1.0.0") OR
      (rabbitmq-c_VERSION VERSION_LESS "0.12.0"))
    list(APPEND DEPS_PRIVATE_C_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
    list(APPEND DEPS_PRIVATE_CXX_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
  endif()
  dump_cmake_variables(REGEX "rabbitmq*" VERBOSE LOG_LEVEL DEBUG)
endif()

########
# Curl #
########

add_yggdrasil_option(
  REST COMM DEPENDENCY CURL PRIVATE
  # SEARCH_ARGS LIBNAMES curl HEADER curl.h
)

##########
# OpenMP #
##########

add_yggdrasil_option(
  OMP DEPENDENCY OpenMP PRIVATE
  FOUND_VAR OpenMP_CXX_FOUND
  LIBRARIES OpenMP::OpenMP_CXX
)

#############
# Threading #
#############

set(THREADS_PREFER_PTHREAD_FLAG TRUE)
set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
cmake_path(SET MINGW_PTHREAD_ROOT "$ENV{MINGW_PTHREAD_ROOT}")
if(MINGW_PTHREAD_ROOT)
  message(DEBUG "MINGW_PTHREAD_ROOT = ${MINGW_PTHREAD_ROOT}")
  add_yggdrasil_option(
    THREADS DEPENDENCY Threads
    LIBRARIES Threads::Threads
    DEFINITIONS -DTHREADSINSTALLED
    DEPENDENCY_PROPERTIES CMAKE_THREAD_LIBS_INIT CMAKE_USE_PTHREADS_INIT
    SEARCH_ARGS PATHS ${MINGW_PTHREAD_ROOT} NO_DEFAULT_PATH
  )
endif()
add_yggdrasil_option(
  THREADS DEPENDENCY Threads
  LIBRARIES Threads::Threads
  DEFINITIONS -DTHREADSINSTALLED
  DEPENDENCY_PROPERTIES CMAKE_THREAD_LIBS_INIT CMAKE_USE_PTHREADS_INIT
)
if (CMAKE_THREAD_LIBS_INIT)
  message(DEBUG "CMAKE_THREAD_LIBS_INIT = ${CMAKE_THREAD_LIBS_INIT}")
  list(APPEND DEPS_PUBLIC_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})
endif()
if (CMAKE_USE_PTHREADS_INIT)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
endif()

#############################
# Patch MinGW 4.3.5 Threads #
#############################

# The version of MinGW provided by conda-forge (5.3.0) does not contain
# the necessary comparison operators for the _M_thread members of
# thread::id. This patch comments out those operators and modifies the
# hash to include both the p & x members of _M_thread.

if (CONDA_PREFIX AND MINGW AND
    CMAKE_CXX_COMPILER_VERSION STREQUAL "5.3.0"
    AND YGG_PATCH_MINGW53_THREAD)
  cmake_path(APPEND CONDA_PREFIX Library mingw-w64 include c++ 5.3.0
             OUTPUT_VARIABLE MINGW_PATHS)
  message(DEBUG "MINGW_PATHS = ${MINGW_PATHS}")
  find_file(THREAD_H thread PATHS ${MINGW_PATHS} NO_DEFAULT_PATH REQUIRED)
  message(DEBUG "THREAD_H = ${THREAD_H}")
  if(NOT THREAD_H STREQUAL THREAD_H-NOTFOUND)
    file(READ ${THREAD_H} THREAD_H_CONTENTS)
    if(THREAD_H_CONTENTS MATCHES "^// mingw\\-c\\+\\+\\-5\\-3\\-thread\\.patch applied.*")
      message(DEBUG "MinGW 5.3.0 thread already patched")
    else()
      message(DEBUG "Patching MinGW 5.3.0 thread")
      message(DEBUG "CONTENTS = ${THREAD_H_CONTENTS}")
      cmake_path(APPEND CMAKE_SOURCE_DIR "mingw-c++-5-3-thread.patch"
                 OUTPUT_VARIABLE THREAD_H_PATCH)
      message(DEBUG "THREAD_H_PATCH = ${THREAD_H_PATCH}")
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

add_yggdrasil_option(MPI COMM LIBRARIES MPI::MPI_CXX PRIVATE)

###################
# Embedded Python #
###################

include(EmbedLanguage)

if(NOT YGGDRASIL_DISABLE_Python_C_API)
    if(YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER)
      set(Python_COMPONENTS Interpreter Development.Module NumPy)
    else()
      set(Python_COMPONENTS Interpreter Development NumPy)
    endif()
    embed_language(Python COMPONENTS ${Python_COMPONENTS})

    # There is an issue with embedding Python on Windows in conda environments
    # https://github.com/ContinuumIO/anaconda-issues/issues/11374
    if(WIN32)
        list(APPEND YGG_TEST_ENV "PYTHONHOME=${Python3_RUNTIME_LIBRARY_DIRS}")
    endif()
    
endif()

#######################
# Select default comm #
#######################

if((NOT YGG_DEFAULT_COMM) AND YGG_COMMS_AVAILABLE)
  list(GET YGG_COMMS_AVAILABLE 0 YGG_DEFAULT_COMM)
  list(APPEND YGG_INSTALL_DEFS -DDEFAULT_COMM_PREFIX=${YGG_DEFAULT_COMM})
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

ygginterface_config_show(DEPS DEBUG)

############################
# Handle interdependencies #
############################

# if(SKBUILD)
#   set(BUILD_Python_LIBRARY ON)
#   if(YGG_LINK_Python_TO_CXX)
#     if(ALLOW_SKBUILD_NONPython)
#       message(FATAL_ERROR "ALLOW_SKBUILD_NONPython is incompatible with YGG_LINK_Python_TO_CXX")
#     endif()
#     set(BUILD_CXX_LIBRARY ON)
#     set(BUILD_Fortran_LIBRARY OFF)
#   elseif(NOT ALLOW_SKBUILD_NONPython)
#     set(BUILD_CXX_LIBRARY OFF)
#     set(BUILD_Fortran_LIBRARY OFF)
#   endif()
#   set(YGG_BUILD_TESTS OFF)
#   set(YGG_BUILD_CXX_TESTS OFF)
#   set(YGG_BUILD_Fortran_TESTS OFF)
#   set(YGG_BUILD_THIRDPARTY_GTEST OFF)
# elseif(BUILD_Python_LIBRARY AND NOT YGGDRASIL_DISABLE_Python_C_API)
#   if(YGG_LINK_Python_TO_CXX)
#     set(BUILD_CXX_LIBRARY ON)
#   endif()
# endif()
if(BUILD_Fortran_LIBRARY AND
   NOT (YGG_Fortran_STANDALONE OR YGG_Fortran_STANDALONE_LINK_EXISTING_CXX))
  set(BUILD_CXX_LIBRARY ON)
endif()
if(YGG_LINK_Python_TO_CXX AND (BUILD_Python_LIBRARY OR SKBUILD))
  set(BUILD_CXX_LIBRARY ON)
endif()

###########################
# Regenerate version file #
###########################

ygginterface_version_header(
  YGGINTERFACE
  ${PROJECT_VERSION}
  ${CMAKE_CURRENT_SOURCE_DIR}/cpp/include/utils/version.hpp.in
  ${CMAKE_CURRENT_SOURCE_DIR}/cpp/include/utils/version.hpp
)

#########################
# Regenerate enum files #
#########################

if (YGG_Fortran_WRAP_C_ENUMS)
  list(
    APPEND GENERATE_ENUM_ARGS --fortran-wrap-c-enums
    --fortran-target ${YGG_TARGET_Fortran}
  )
endif()
python_code_generation(
  enum utils/generate_enum.py
  ARGUMENTS ${GENERATE_ENUM_ARGS}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  SOURCES
  cpp/include/utils/enums.hpp
  ${YGG_TARGET_Fortran_NAME}
  BYPRODUCTS
  cpp/include/utils/enums_maps.hpp
  cpp/src/utils/enums_maps.cpp
  fortran/c_wrappers_enums.h
  fortran/c_wrappers_enums.c
  fortran/YggInterface_enums.F90
)

########################################
# Generate yggdrasil_rapidjson wrapper #
########################################

set(GENERATE_RJWRAPPER_ARGS)
# if(WRAP_YGGDRASIL_RAPIDJSON_FOR_DLL)
#   list(APPEND GENERATE_RJWRAPPER_ARGS --wrap-yggdrasil-rapidjson)
# endif()
if(GENERATE_YGGDRASIL_RAPIDJSON_WRAPPER)
  python_code_generation(
    rjwrapper utils/generate_generic
    ENTRY_POINT -m generate_generic.runner
    ARGUMENTS --language=rjwrapper ${GENERATE_RJWRAPPER_ARGS}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/utils
    SOURCES
    ${YggdrasilRapidJSON_INCLUDE_DIRS}/yggdrasil_rapidjson/document.h
    BYPRODUCTS
    ${CMAKE_SOURCE_DIR}/cpp/include/utils/yggdrasil_rapidjson_wrapper2.hpp
    ${CMAKE_SOURCE_DIR}/cpp/src/utils/yggdrasil_rapidjson_wrapper2.cpp
  )
endif()

################################################
# Generate data types interface in C & Fortran #
################################################

if(GENERATE_INTERFACE)
  list(APPEND GENERATE_DATATYPES_ARGS
       --fortran-target ${YGG_TARGET_Fortran})
  python_code_generation(
    cfort utils/generate_generic
    ENTRY_POINT -m generate_generic.runner
    ARGUMENTS ${GENERATE_DATATYPES_ARGS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/utils
    SOURCES
    cpp/include/YggInterface.h
    cpp/include/communicators/comm_t.hpp
    ${YGG_TARGET_Fortran_NAME}
    BYPRODUCTS
    cpp/src/datatypes/dtype_t.cpp
    cpp/include/datatypes/dtype_t.h
    fortran/YggInterface.F90
    fortran/YggInterface_cdef.F90
    fortran/c_wrappers.h
    fortran/c_wrappers.c
    fortran/YggInterface_interfaces.F90
  )
endif()

######################
# Debug messages     #
######################

dump_cmake_variables(
  LOG_LEVEL DEBUG VARIABLES
  DEPS_PUBLIC_COMPILE_FLAGS
  DEPS_PUBLIC_C_COMPILE_FLAGS DEPS_PUBLIC_CXX_COMPILE_FLAGS
  DEPS_PUBLIC_LINK_FLAGS
  DEPS_PUBLIC_C_LINK_FLAGS DEPS_PUBLIC_CXX_LINK_FLAGS
  DEPS_PUBLIC_LIBRARIES DEPS_PRIVATE_LIBRARIES
  DEPS_PUBLIC_C_LIBS DEPS_PUBLIC_CXX_LIBS
  DEPS_PUBLIC_Fortran_LIBS
  DEPS_Python_LIBRARIES
  DEPS_LIBRARY_DIRS DEPS_INCLUDE_DIRS
  YGG_INSTALL_DEPS YGG_INSTALL_DEFS
  YGG_OPTIONS_SUPPORTED YGG_COMMS_SUPPORTED YGG_COMMS_AVAILABLE
  YGG_LANGUAGES_SUPPORTED YGG_LANGUAGES_AVAILABLE YGG_RPATH
)
foreach(libtype PUBLIC PRIVATE Python "")
  if(libtype)
    set(ilibvar DEPS_LIBRARIES)
  else()
    set(ilibvar DEPS_${libtype}_LIBRARIES)
  endif()
  foreach(ilib IN LISTS ${ilibvar})
    if(TARGET ${ilib})
      get_target_property(ilink_libraries ${ilib} INTERFACE_LINK_LIBRARIES)
      get_target_property(iimport_location ${ilib} IMPORTED_LOCATION)
      if(ilink_libraries STREQUAL "ilink_libraries-NOT-FOUND")
        set(ilink_libraries)
      endif()
      if(iimport_location STREQUAL "iimport_location-NOT-FOUND")
        set(iimport_location)
      endif()
      message(DEBUG "TARGET DEP LIBRARIES ${ilib}[${libtype}]: \"${iimport_location}\", \"${ilink_libraries}\"")
    else()
      message(DEBUG "TARGET DEP LIBRARIES DIRECT[${libtype}]: ${ilib}")
    endif()
  endforeach()
endforeach()

######################
# Create C++ Library #
######################

file(GLOB_RECURSE SOURCES_CXX "cpp/*.cpp")
if (NOT SOURCES_CXX)
  message(FATAL_ERROR "Sources could not be located")
endif()
add_yggdrasil_interface(CXX DIRECTORY cpp)

#########################
# Create Python Library #
#########################

add_yggdrasil_interface(Python DISABLE_BY_DEFAULT)

##########################
# Create Fortran Library #
##########################

if(CMAKE_Fortran_COMPILER AND WIN32)
  cmake_path(SET CMAKE_Fortran_COMPILER NORMALIZE ${CMAKE_Fortran_COMPILER})
endif()
add_yggdrasil_interface(Fortran TARGET ${YGG_TARGET_Fortran})

##################
# Install        #
##################

include(InstallTools)
set(CONDITIONAL_INSTALL_ARGS)
if(NOT YGG_BUILD_DOCS)
  list(APPEND CONDITIONAL_INSTALL_ARGS DONT_INSTALL_DOCS)
endif()
if(YGG_INSTALL_TARGETS)
  message(DEBUG "Install enabled: YGG_INSTALL_TARGETS = ${YGG_INSTALL_TARGETS}")
  ygginterface_options_export()
  complete_install(
    ${PROJECT_NAME} TARGETS ${YGG_INSTALL_TARGETS}
    POSSIBLE_TARGETS ${YGG_INSTALL_TARGETS_SUPPORTED}
    VERSION ${YGGINTERFACE_VERSION}
    HEADERS ${YGG_INSTALL_INCLUDES}
    # HEADER_DIR cpp/include/
    NESTED_INCLUDEDIR
    MODULE_DIR "${CMAKE_CURRENT_LIST_DIR}/cmake"
    CONFIG_TEMPLATE_DIR ${CMAKE_CURRENT_LIST_DIR}/cmake/config
    MODULE_SCRIPT_DIR ${CMAKE_CURRENT_LIST_DIR}/cmake/scripts
    DOC_DIR ${DOXYGEN_BUILD_DIR}/
    EXPORT_TARGETS_INDEPENDENTLY
  )
else()
  message(DEBUG "Install disabled")
endif()
    
#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS OR YGG_BUILD_Fortran_TESTS OR YGG_BUILD_Python_TESTS)
    enable_testing()
    include(CTest)
    add_subdirectory(tests)
endif()
