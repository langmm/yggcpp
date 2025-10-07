cmake_minimum_required(VERSION 3.16)

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

if (NOT DEFAULT_RAPIDJSON_INCLUDE_DIRS)
  cmake_path(APPEND CMAKE_CURRENT_SOURCE_DIR cpp include rapidjson include
             OUTPUT_VARIABLE DEFAULT_RAPIDJSON_INCLUDE_DIRS)
endif()
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(ygg_options)

option(BUILD_CPP_LIBRARY "Build the Yggdrasil C++ interface library" ON)
option(BUILD_FORTRAN_LIBRARY "Build the Yggdrasil Fortran interface library" ON)
option(BUILD_PYTHON_LIBRARY "Build the Yggdrasil Python interface library" OFF)
option(YGG_PYTHON_LIBRARY_WRAP "Build the Yggdrasil Python interface library by wrapping the C++ module" ON)
option(YGG_Fortran_REQUIRED "Raise an error if the Fortran interface library cannot be build" OFF)
option(ALLOW_SKBUILD_NONPYTHON "Allow the non-Python interfaces (C/C++/Fortran) to be built from SKBUILD" OFF)
option(ENABLE_ZMQ "Enable ZeroMQ" ON)
option(ENABLE_RMQ "Enable RabbitMQ" ON)
option(ENABLE_REST "Enable REST API" ON)
option(USING_IPC "Default to using IPC V queues" OFF)
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
option(WRAP_RAPIDJSON_FOR_DLL "Build yggdrasil using a wrapper for rapidjson to force it into a DLL (enabled automatically when compiling with MSVC" OFF)
option(YGG_LINK_PYTHON_TO_CPP "Link the Python extension to the C++ wrapper as a library" OFF)
option(YGG_BUILD_DOCS "Build documentation (requires Doxygen)" OFF)
option(DOXYGEN_CHECK_MISSING "Raise an error if documentation is missing" OFF)
option(YGG_PATCH_MINGW53_THREAD "Patch the MinGW 5.3.0 thread header" ON)
option(YGG_FORTRAN_WRAP_C_ENUMS "Wrap the int64_t enumerators in a C layer for binding in Fortran" OFF)

list(APPEND DEPS_PUBLIC_C_COMPILE_FLAGS ${ASAN_COMPILE_FLAGS})
list(APPEND DEPS_PUBLIC_CXX_COMPILE_FLAGS ${ASAN_COMPILE_FLAGS})
list(APPEND DEPS_PUBLIC_C_LINK_FLAGS ${ASAN_LINK_FLAGS})
list(APPEND DEPS_PUBLIC_CXX_LINK_FLAGS ${ASAN_LINK_FLAGS})

include(FetchContent)

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
        install(DIRECTORY ${DOXYGEN_BUILD_DIR}/
                DESTINATION ${CMAKE_INSTALL_PREFIX}/share/doc/ygginterface
                OPTIONAL)
    endif()
endif()

#########################################
# DEPENDENCIES
#########################################

cmake_path(SET CONDA_PREFIX "$ENV{CONDA_PREFIX}")
if(CONDA_PREFIX)
    message(STATUS "CONDA_PREFIX = ${CONDA_PREFIX}")
endif()

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
	# execute_process(
	#   COMMAND ls ${ZeroMQ_INCLUDE_DIR}
	#   COMMAND_ECHO STDOUT)
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
        list(APPEND YGG_INSTALL_DEFS -DRMQINSTALLED)
        list(APPEND YGG_INSTALL_DEPS rabbitmq-c)
        if (rabbitmq-c_CONFIG)
          include(${rabbitmq-c_CONFIG})
  	  list(APPEND DEPS_LIBRARIES rabbitmq::rabbitmq)
	  execute_process(
	    COMMAND ls ${rabbitmq-c_DIR}
	    COMMAND_ECHO STDOUT)
	else()
	  list(APPEND DEPS_LIBRARIES ${rabbitmq-c_LIBRARY})
	  list(APPEND DEPS_INCLUDE_DIRS ${rabbitmq-c_INCLUDE_DIR})
	endif()
	# In v0.12.0 (v9.2.5 so), the include files were moved into a
	# dedicated rabbitmq-c directory. This is complicated by the
        # so version (and cmake version) not matching the source version.
	if (rabbitmq-c_VERSION)
	  message(STATUS "rabbitmq-c_VERSION = ${rabbitmq-c_VERSION}")
	  if (rabbitmq-c_DIR MATCHES ".*vcpkg.*")
	    message(STATUS "rabbitmq-c_DIR MATCHES *vcpkg*")
	  else()
	    message(STATUS "rabbitmq-c_DIR DOES NOT MATCH *vcpkg*")
	  endif()
	  if (rabbitmq-c_VERSION VERSION_LESS "0.12.0")
	    message(STATUS "rabbitmq-c_VERSION LESS 0.12.0")
	  else()
	    message(STATUS "rabbitmq-c_VERSION MORE 0.12.0")
	  endif()
	  # if (rabbitmq-c_VERSION VERSION_LESS "9.2.5")
	  #   message(STATUS "rabbitmq-c_VERSION LESS 9.2.5")
	  # else()
	  #   message(STATUS "rabbitmq-c_VERSION MORE 9.2.5")
	  # endif()
	else()
	  message(STATUS "rabbitmq-c_VERSION NOT SET")
	endif()
	if ((NOT rabbitmq-c_VERSION) OR
            (rabbitmq-c_VERSION VERSION_GREATER "1.0.0") OR
	    (rabbitmq-c_VERSION VERSION_LESS "0.12.0"))
          list(APPEND DEPS_PUBLIC_C_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
	  list(APPEND DEPS_PUBLIC_CXX_COMPILE_FLAGS -DYGG_RMQ_NOINCLUDEDIR)
	endif()
    else()
        message(STATUS "NO RabbitMQ client library")
    endif()
    dump_cmake_variables(REGEX "rabbitmq*" VERBOSE)
else (ENABLE_RMQ)
    message(STATUS "No RabbitMQ support will be built." )
endif (ENABLE_RMQ)

########
# Curl #
########

if (ENABLE_REST)
  find_package(CURL)
  # find_package_pkgconfig(CURL LIBNAMES curl HEADER curl.h)
  if (CURL_FOUND)
    list(APPEND YGG_INSTALL_DEFS -DRESTINSTALLED)
    list(APPEND YGG_INSTALL_DEPS CURL)
    list(APPEND DEPS_LIBRARIES ${CURL_LIBRARIES})
    list(APPEND DEPS_INCLUDE_DIRS ${CURL_INCLUDE_DIRS})
  else()
    message(STATUS "NO curl library")
  endif()
else (ENABLE_REST)
  message(STATUS "No REST support will be built.")
endif(ENABLE_REST)

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
    CMAKE_CXX_COMPILER_VERSION STREQUAL "5.3.0"
    AND YGG_PATCH_MINGW53_THREAD)
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
      message(STATUS "CONTENTS = ${THREAD_H_CONTENTS}")
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
        set(RAPIDJSON_PYTHON_ENV2 "PYTHONHOME=${${PYTHON_PREFIX}_RUNTIME_LIBRARY_DIRS}")
    endif()
    
    list(APPEND YGG_INSTALL_DEPS ${PYTHON_PREFIX})
    list(APPEND DEPS_LIBRARIES ${PYTHON_PREFIX}::NumPy)
    list(APPEND DEPS_LIBRARIES_PYTHON ${PYTHON_PREFIX}::Python)
    if(YGGDRASIL_PYGIL_NO_MANAGEMENT)
      list(APPEND YGG_INSTALL_DEFS -DYGGDRASIL_PYGIL_NO_MANAGEMENT)
    endif()
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

############################
# Handle interdependencies #
############################

if(SKBUILD)
  set(BUILD_PYTHON_LIBRARY ON)
  if(YGG_LINK_PYTHON_TO_CPP)
    if(ALLOW_SKBUILD_NONPYTHON)
      message(FATAL_ERROR "ALLOW_SKBUILD_NONPYTHON is incompatible with YGG_LINK_PYTHON_TO_CPP")
    endif()
    set(BUILD_CPP_LIBRARY ON)
    set(BUILD_FORTRAN_LIBRARY OFF)
  elseif(NOT ALLOW_SKBUILD_NONPYTHON)
    set(BUILD_CPP_LIBRARY OFF)
    set(BUILD_FORTRAN_LIBRARY OFF)
  endif()
  set(YGG_BUILD_TESTS OFF)
  set(YGG_BUILD_CXX_TESTS OFF)
  set(YGG_BUILD_FORTRAN_TESTS OFF)
  set(YGG_BUILD_THIRDPARTY_GTEST OFF)
elseif(BUILD_PYTHON_LIBRARY AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
  if(YGG_LINK_PYTHON_TO_CPP)
    set(BUILD_CPP_LIBRARY ON)
  endif()
endif()
if(BUILD_FORTRAN_LIBRARY)
  set(BUILD_CPP_LIBRARY ON)
endif()
set(BUILD_C_LIBRARY ${BUILD_CPP_LIBRARY})

##################
# Coverage       #
##################

message(STATUS "CMAKE_CURRENT_BINARY_DIR = ${CMAKE_CURRENT_BINARY_DIR}")
if(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
  find_program(GCOVR_FOUND gcovr)
  if (GCOVR_FOUND)
    message(STATUS "COVERAGE ON")
    set(COVERAGE_DIR ${CMAKE_BINARY_DIR}/coverage)
    file(MAKE_DIRECTORY ${COVERAGE_DIR})
    list(APPEND COVERAGE_C_COMPILE_FLAGS -g -O0 --coverage -fno-inline -fprofile-arcs -ftest-coverage)
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
      set(CTEST_COVERAGE_COMMAND "llvm-cov")
      set(CTEST_COVERAGE_EXTRA_FLAGS "gcov")
      list(APPEND COVERAGE_C_COMPILE_FLAGS -fprofile-instr-generate -fcoverage-mapping)
    else()
      list(APPEND COVERAGE_C_LIBRARIES gcov)
      set(CTEST_COVERAGE_COMMAND "gcov")
      set(CTEST_COVERAGE_EXTRA_FLAGS "")
      list(APPEND COVERAGE_C_COMPILE_FLAGS -fno-inline-small-functions -fno-default-inline)
    endif()
    set(COVERAGE_C_LINK_FLAGS ${COVERAGE_C_COMPILE_FLAGS})
  else()
    message(STATUS "Could not find gcovr, coverage disabled")
    set(YGG_ENABLE_COVERAGE OFF)
  endif()
endif(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")

function(cover_test target)
  if(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
    add_test(NAME ${target}_coverage
             COMMAND sh -c "gcovr --root=${CMAKE_SOURCE_DIR} --gcov-executable=\"${CTEST_COVERAGE_COMMAND} ${CTEST_COVERAGE_EXTRA_FLAGS}\" --coveralls=${COVERAGE_DIR}/coverage.info --html-details=${COVERAGE_DIR}/coverage.html --exclude=cpp/include/rapidjson/ --exclude=tests/ --exclude=thirdparty/ --filter=${CMAKE_SOURCE_DIR} -v ${CMAKE_BINARY_DIR}"
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_tests_properties(${target}_coverage PROPERTIES DEPENDS "${target}")
  endif(YGG_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
endfunction()

#########################
# Regenerate enum files #
#########################

if (${PYTHON_PREFIX}_EXECUTABLE)
  if (YGG_FORTRAN_WRAP_C_ENUMS)
    list(APPEND GENERATE_ENUM_ARGS --fortran-wrap-c-enums)
  endif()
  execute_process(
    COMMAND ${${PYTHON_PREFIX}_EXECUTABLE} utils/generate_enum.py ${GENERATE_ENUM_ARGS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND_ERROR_IS_FATAL ANY)
endif()

################################################
# Generate data types interface in C & Fortran #
################################################

if (${PYTHON_PREFIX}_EXECUTABLE)
  execute_process(
    COMMAND ${${PYTHON_PREFIX}_EXECUTABLE} utils/generate_generic.py ${GENERATE_DATATYPES_ARGS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND_ERROR_IS_FATAL ANY)
endif()

######################
# Create C++ Library #
######################

file(GLOB_RECURSE SOURCES_CPP "cpp/src/*.cpp")
if (NOT SOURCES_CPP)
  message(FATAL_ERROR "Sources could not be located")
endif()

if(BUILD_CPP_LIBRARY)
    add_subdirectory(cpp)
endif()

#########################
# Create Python Library #
#########################

if(BUILD_PYTHON_LIBRARY)
    add_subdirectory(python)
endif()

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
  
  if (SKBUILD)
    set(YGGDRASIL_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_INCLUDEDIR})
  else()
    cmake_path(APPEND CMAKE_INSTALL_INCLUDEDIR YggInterface
               OUTPUT_VARIABLE YGGDRASIL_INSTALL_INCLUDEDIR)
  endif()

  install(TARGETS ${YGG_INSTALL_TARGETS}
          EXPORT YggInterfaceTargets
	  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
	  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
	  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
	  PUBLIC_HEADER DESTINATION ${YGGDRASIL_INSTALL_INCLUDEDIR})
  install(EXPORT YggInterfaceTargets
          FILE YggInterfaceTargets.cmake
	  NAMESPACE YggInterface::
	  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/YggInterface)
  install(DIRECTORY cpp/include/
          DESTINATION ${YGGDRASIL_INSTALL_INCLUDEDIR})
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
            DESTINATION ${YGGDRASIL_INSTALL_INCLUDEDIR})
  endif()
endif ()
    
#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS OR YGG_BUILD_FORTRAN_TESTS OR YGG_BUILD_PYTHON_TESTS)
    enable_testing()
    include(CTest)
    add_subdirectory(tests)
endif()
