# This file provides macros for compilation options that might be used
# by dependent packages and uses macros from the YggdrasilRapidJSON
# library configuration file if it is found

macro(ygginterface_options OUTPUT_VARIABLE)
  set(${OUTPUT_VARIABLE})
  if(YggdrasilRapidJSON_FOUND)
    yggdrasil_rapidjson_options(${OUTPUT_VARIABLE})
  endif()
  list(
    APPEND ${OUTPUT_VARIABLE}
    YGG_BUILD_ASAN
    YGG_BUILD_UBSAN
    YGG_ENABLE_INSTRUMENTATION_OPT
    YGG_DEBUG_LEVEL
    YGGDRASIL_RAPIDJSON_INCLUDE_DIRS
    YGG_DEFAULT_COMM
    YGG_LINK_Python_TO_CXX
    WRAP_YGGDRASIL_RAPIDJSON_FOR_DLL
  )
endmacro()

macro(ygginterface_options_values OUTPUT_VARIABLE)
  ygginterface_options(YGGINTERFACE_OPTIONS_NAMES)
  set(${OUTPUT_VARIABLE})
  foreach(var IN LISTS YGGINTERFACE_OPTIONS_NAMES)
    list(APPEND ${OUTPUT_VARIABLE} ${var} "${${var}}")
    message(STATUS "${OUTPUT_VARIABLE} = ${${OUTPUT_VARIABLE}}")
  endforeach()
endmacro()

macro(ygginterface_options_export)
  ygginterface_options_values(YGG_INSTALL_CONFIG)
endmacro()

macro(ygginterface_options_import)
  list(LENGTH YGG_INSTALL_CONFIG N_YGG_INSTALL_CONFIG)
  foreach(i RANGE 0 ${N_YGG_INSTALL_CONFIG}-2 2)
    if(${i} GREATER_EQUAL ${N_YGG_INSTALL_CONFIG})
      break()
    endif()
    math(EXPR ip1 "${i}+1")
    list(GET YGG_INSTALL_CONFIG ${i} ikey)
    list(GET YGG_INSTALL_CONFIG ${ip1} ival)
    set(${ikey} ${ival})
  endforeach()
endmacro()

macro(ygginterface_options_create)
  option(YGG_BUILD_ASAN "Build with address sanitizer (gcc/clang)" OFF)
  option(YGG_BUILD_UBSAN "Build with undefined behavior sanitizer (gcc/clang)" OFF)
  option(YGG_ENABLE_INSTRUMENTATION_OPT "Build yggdrasil with -march or -mcpu options" ON)
  option(YGG_DEBUG_LEVEL "Level that should be used for logging" OFF)
  set(YGGDRASIL_RAPIDJSON_INCLUDE_DIRS "" CACHE PATH "Path to the yggdrasil_rapidjson include directory containing the headers that should be used")
  set(YGG_DEFAULT_COMM "ZMQ" CACHE STRING "Default communicator")
  option(YGG_LINK_Python_TO_CXX "Link the Python extension to the C++ interface library" OFF)
  option(WRAP_YGGDRASIL_RAPIDJSON_FOR_DLL "Build yggdrasil using a wrapper for yggdrasil_rapidjson to force it into a DLL (enabled automatically when compiling with MSVC" OFF)
  # TODO
  if(NOT YGGDRASIL_RAPIDJSON_INCLUDE_DIRS)
    find_package(YggdrasilRapidJSON)
    message(STATUS "YggdrasilRapidJSON_VERSION = ${YggdrasilRapidJSON_VERSION}")
  endif()
  if(YggdrasilRapidJSON_FOUND)
    yggdrasil_rapidjson_options_create()
  endif()
endmacro()

macro(ygginterface_options_config OUTPUT_PREFIX)
  foreach(flag BUILD_ASAN BUILD_UBSAN ENABLE_INSTRUMENTATION_OPT)
    set(YGGDRASIL_RAPIDJSON_${flag} ${YGG_${flag}})
  endforeach()
  if(WIN32 AND CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(WRAP_YGGDRASIL_RAPIDJSON_FOR_DLL ON)
  endif()
  if(WRAP_YGGDRASIL_RAPIDJSON_FOR_DLL)
    set(YGG_LINK_Python_TO_CXX OFF)
  endif()
  if(SKBUILD)
    set(YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER ON)
  endif()
  if(YggdrasilRapidJSON_FOUND)
    yggdrasil_rapidjson_options_config(${OUTPUT_PREFIX})
  endif()
  if(YGG_DEBUG_LEVEL)
    list(
      APPEND ${OUTPUT_PREFIX}_PUBLIC_COMPILE_FLAGS
      -DYGG_DEBUG=${YGG_DEBUG_LEVEL}
    )
  endif()
  if(YGGDRASIL_RAPIDJSON_INCLUDE_DIRS)
    list(
      APPEND ${OUTPUT_PREFIX}_INCLUDE_DIRECTORIES
      "${YGGDRASIL_RAPIDJSON_INCLUDE_DIRS}"
    )
    list(
      APPEND ${OUTPUT_PREFIX}_PUBLIC_COMPILE_FLAGS
      -DYGGDRASIL_RAPIDJSON_HAS_STDSTRING
      -DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION
      -D_USE_MATH_DEFINES
    )
  else()
    find_package(YggdrasilRapidJSON REQUIRED)
    # Remove Python as dependency so that this target can be used
    # as part of a Python C extension
    include(BuildTools)
    strip_python(YggdrasilRapidJSON)
    list(
      APPEND ${OUTPUT_PREFIX}_LIBRARIES
      YggdrasilRapidJSON
    )
  endif()
  if((NOT YGG_DEFAULT_COMM) AND YGG_COMMS_AVAILABLE)
    list(GET YGG_COMMS_AVAILABLE 0 YGG_DEFAULT_COMM)
  endif()
  if(YGG_DEFAULT_COMM)
    list(
      APPEND ${OUTPUT_PREFIX}_PUBLIC_COMPILE_FLAGS
      -DDEFAULT_COMM_PREFIX=${YGG_DEFAULT_COMM}
    )
  endif()
  if(YGG_LINK_Python_TO_CXX)
    list(
      APPEND ${OUTPUT_PREFIX}_PUBLIC_COMPILE_FLAGS
      -DYGG_LINK_PYTHON_TO_CPP
    )
  endif()
  if(WRAP_YGGDRASIL_RAPIDJSON_FOR_DLL)
    list(
      APPEND ${OUTPUT_PREFIX}_PUBLIC_COMPILE_FLAGS
      -DWRAP_YGGDRASIL_RAPIDJSON_FOR_DLL
    )
  endif()
endmacro()

macro(ygginterface_gitversion OUTPUT_VARIABLE DEFAULT)
  find_package(Git)
  if(NOT Git_FOUND)
    message(STATUS "Failed to find Git cmake package, falling back to version ${DEFAULT}")
    set(${OUTPUT_VARIABLE} ${DEFAULT})
  else()
    # Generate a git-describe version string from Git repository tags
    execute_process(
      COMMAND ${GIT_EXECUTABLE} describe --tags --dirty --match "v*"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE GIT_DESCRIBE_VERSION
      RESULT_VARIABLE GIT_DESCRIBE_ERROR_CODE
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if(GIT_DESCRIBE_ERROR_CODE)
      message(STATUS "Error getting git tag, falling back to version ${DEFAULT}")
      set(${OUTPUT_VARIABLE} ${DEFAULT})
    else()
      string(SUBSTRING "${GIT_DESCRIBE_VERSION}" 1 -1 GIT_DESCRIBE_VERSION)
      string(FIND "${GIT_DESCRIBE_VERSION}" "-" idx)
      if(NOT ${idx} EQUAL "-1")
        string(SUBSTRING "${GIT_DESCRIBE_VERSION}" 0 ${idx} GIT_DESCRIBE_VERSION)
        string(FIND "${GIT_DESCRIBE_VERSION}" "." idx REVERSE)
        math(EXPR idxp1 "${idx}+1")
        string(SUBSTRING "${GIT_DESCRIBE_VERSION}" ${idxp1} -1 EXTEN_VERSION)
        math(EXPR EXTEN_VERSION "${EXTEN_VERSION}+1")
        string(SUBSTRING "${GIT_DESCRIBE_VERSION}" 0 ${idx} GIT_DESCRIBE_VERSION)
        set(GIT_DESCRIBE_VERSION "${GIT_DESCRIBE_VERSION}.${EXTEN_VERSION}")
      endif()
      set(${OUTPUT_VARIABLE} ${GIT_DESCRIBE_VERSION})
    endif()
  endif()
endmacro()

function(ygginterface_version_header MACRO_PREFIX VERSION_STRING SRC DST)
  set(rem ${VERSION_STRING})
  set(idx 0)
  set(parts)
  while(NOT ${idx} EQUAL "-1")
    string(FIND "${rem}" "." idx)
    if(${idx} EQUAL "-1")
      list(APPEND parts "${rem}")
    else()
      string(SUBSTRING "${rem}" 0 ${idx} part)
      list(APPEND parts "${part}")
      math(EXPR idxp1 "${idx}+1")
      string(SUBSTRING "${rem}" ${idxp1} -1 rem)
    endif()
  endwhile()
  list(GET parts 0 MAJOR_VERSION)
  list(GET parts 1 MINOR_VERSION)
  list(GET parts 2 PATCH_VERSION)
  configure_file(${SRC} ${DST} @ONLY)
endfunction()
