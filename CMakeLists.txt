cmake_minimum_required(VERSION 3.18)
message(DEBUG "TOP LEVEL CMAKE_GENERATOR=${CMAKE_GENERATOR}")

SET(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules")
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/cmake")

set(YGGINTERFACE_VERSION "" CACHE STRING "Version to use instead of the git tag version")
include(YggInterfaceMacros)
if(NOT YGGINTERFACE_VERSION)
  ygginterface_gitversion(YGGINTERFACE_VERSION "0.1.0")
endif()
message(STATUS "YGGINTERFACE_VERSION = ${YGGINTERFACE_VERSION}")

cmake_policy(SET CMP0048 NEW)
PROJECT(YggInterface VERSION "${YGGINTERFACE_VERSION}")

include(ygg_options)
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


##################################
# Option dependent configuration #
##################################

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
    message(STATUS "CONDA_PREFIX = ${CONDA_PREFIX}")
endif()

######################
# YggdrasilRapidJSON #
######################

if(NOT YGGDRASIL_RAPIDJSON_INCLUDE_DIRS)
  add_yggdrasil_dependency(
    YggdrasilRapidJSON
    LIBRARIES YggdrasilRapidJSON
    REQUIRED
  )
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

add_yggdrasil_option(ZMQ COMM DEPENDENCY ZeroMQ)

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

add_yggdrasil_option(RMQ COMM DEPENDENCY rabbitmq-c)
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
    list(APPEND DEPS_PUBLIC_C_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
    list(APPEND DEPS_PUBLIC_CXX_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
  endif()
  dump_cmake_variables(REGEX "rabbitmq*" VERBOSE)
endif()

########
# Curl #
########

add_yggdrasil_option(
  REST COMM DEPENDENCY CURL
  # SEARCH_ARGS LIBNAMES curl HEADER curl.h
)

##########
# OpenMP #
##########

add_yggdrasil_option(
  OMP DEPENDENCY OpenMP
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
  list(APPEND DEPS_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})
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

add_yggdrasil_option(MPI COMM LIBRARIES MPI::MPI_CXX)

###################
# Embedded Python #
###################

include(EmbedLanguage)

if(NOT YGGDRASIL_DISABLE_Python_C_API)
    embed_language(Python)

    # There is an issue with embedding Python on Windows in conda environments
    # https://github.com/ContinuumIO/anaconda-issues/issues/11374
    if(WIN32)
        list(APPEND YGG_TEST_ENV "PYTHONHOME=${${Python_PREFIX}_RUNTIME_LIBRARY_DIRS}")
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

message(DEBUG "DEPS_INCLUDE_DIRS = ${DEPS_INCLUDE_DIRS}")
message(DEBUG "DEPS_LIBRARIES = ${DEPS_LIBRARIES}")
message(DEBUG "DEPS_LIBRARY_DIRS = ${DEPS_LIBRARY_DIRS}")

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
if(BUILD_Fortran_LIBRARY)
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
  list(APPEND GENERATE_ENUM_ARGS --fortran-wrap-c-enums)
endif()
python_code_generation(
  enum utils/generate_enum.py
  ARGUMENTS ${GENERATE_ENUM_ARGS}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  SOURCES cpp/include/utils/enums.hpp
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
if(YGGDRASIL_RAPIDJSON_INCLUDE_DIRS)
  list(APPEND GENERATE_RJWRAPPER_ARGS --yggdrasil-rapidjson-include-dirs=${YGGDRASIL_RAPIDJSON_INCLUDE_DIRS})
endif()
if(GENERATE_YGGDRASIL_RAPIDJSON_WRAPPER)
  python_code_generation(
    rjwrapper utils/generate_generic
    ENTRY_POINT -m generate_generic.runner
    ARGUMENTS --language=rjwrapper ${GENERATE_RJWRAPPER_ARGS}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/utils
    SOURCES
    ${YGGDRASIL_RAPIDJSON_INCLUDE_DIRS}/yggdrasil_rapidjson/document.h
    BYPRODUCTS
    ${CMAKE_SOURCE_DIR}/cpp/include/utils/yggdrasil_rapidjson_wrapper2.hpp
    ${CMAKE_SOURCE_DIR}/cpp/src/utils/yggdrasil_rapidjson_wrapper2.cpp
  )
endif()

################################################
# Generate data types interface in C & Fortran #
################################################

if(GENERATE_INTERFACE)
  python_code_generation(
    cfort utils/generate_generic
    ENTRY_POINT -m generate_generic.runner
    ARGUMENTS ${GENERATE_DATATYPES_ARGS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/utils
    SOURCES
    cpp/include/YggInterface.h
    cpp/include/communicators/comm_t.hpp
    BYPRODUCTS
    cpp/src/datatypes/dtype_t.cpp
    cpp/include/datatypes/dtype_t.h
    fortran/YggInterface.F90
    fortran/YggInterface_cdef.F90
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
  DEPS_LIBRARIES DEPS_LIBRARIES_PRIVATE
  DEPS_PUBLIC_C_LIBS DEPS_PUBLIC_CXX_LIBS
  DEPS_PUBLIC_Fortran_LIBS
  DEPS_LIBRARIES_Python
  DEPS_LIBRARY_DIRS DEPS_INCLUDE_DIRS
  YGG_INSTALL_DEPS YGG_INSTALL_DEFS
  YGG_OPTIONS_SUPPORTED YGG_COMMS_SUPPORTED YGG_COMMS_AVAILABLE
  YGG_LANGUAGES_SUPPORTED YGG_LANGUAGES_AVAILABLE YGG_RPATH
)
foreach(ilib IN LISTS DEPS_LIBRARIES DEPS_LIBRARIES_Python)
  if(TARGET ${ilib})
    get_target_property(ilink_libraries ${ilib} INTERFACE_LINK_LIBRARIES)
    get_target_property(iimport_location ${ilib} IMPORTED_LOCATION)
    if(ilink_libraries STREQUAL "ilink_libraries-NOT-FOUND")
      set(ilink_libraries)
    endif()
    if(iimport_location STREQUAL "iimport_location-NOT-FOUND")
      set(iimport_location)
    endif()
    message(DEBUG "TARGET DEP LIBRARIES ${ilib}: \"${iimport_location}\", \"${ilink_libraries}\"")
  else()
    message(DEBUG "TARGET DEP LIBRARIES DIRECT: ${ilib}")
  endif()
endforeach()

######################
# Create C++ Library #
######################

file(GLOB_RECURSE SOURCES_CXX "cpp/src/*.cpp")
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
add_yggdrasil_interface(Fortran)

##################
# Install        #
##################

include(InstallTools)
message(DEBUG "YGG_INSTALL_TARGETS = ${YGG_INSTALL_TARGETS}")
set(CONDITIONAL_INSTALL_ARGS)
if(NOT YGG_BUILD_DOCS)
  list(APPEND CONDITIONAL_INSTALL_ARGS DONT_INSTALL_DOCS)
endif()
if(YGG_INSTALL_TARGETS)
  message(STATUS "Install enabled")
  ygginterface_options_export()
  complete_install(
    ${PROJECT_NAME} TARGETS ${YGG_INSTALL_TARGETS}
    VERSION ${YGGINTERFACE_VERSION}
    HEADERS ${YGG_INSTALL_INCLUDES}
    HEADER_DIR cpp/include/ NESTED_INCLUDEDIR
    MODULE_DIR "${CMAKE_CURRENT_LIST_DIR}/cmake"
    CONFIG_TEMPLATE_DIR ${CMAKE_CURRENT_LIST_DIR}/cmake/config
    MODULE_SCRIPT_DIR ${CMAKE_CURRENT_LIST_DIR}/cmake/scripts
    DOC_DIR ${DOXYGEN_BUILD_DIR}/
  )
else()
  message(STATUS "Install disabled")
endif()
    
#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS OR YGG_BUILD_Fortran_TESTS OR YGG_BUILD_Python_TESTS)
    enable_testing()
    include(CTest)
    add_subdirectory(tests)
endif()
