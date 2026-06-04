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
    YGG_ENABLE_COVERAGE
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
  option(YGG_ENABLE_COVERAGE "Enable coverage reporting" OFF)
  option(YGG_DEBUG_LEVEL "Level that should be used for logging" OFF)
  set(YGGDRASIL_RAPIDJSON_INCLUDE_DIRS "" CACHE PATH "Path to the yggdrasil_rapidjson include directory containing the headers that should be used")
  set(YGG_DEFAULT_COMM "" CACHE STRING "Default communicator")
  option(YGG_LINK_Python_TO_CXX "Link the Python extension to the C++ interface library" OFF)
  option(WRAP_YGGDRASIL_RAPIDJSON_FOR_DLL "Build yggdrasil using a wrapper for yggdrasil_rapidjson to force it into a DLL (enabled automatically when compiling with MSVC" OFF)
  option(YGGDRASIL_DISABLE_Python_C_API "Disable the Python C API in YggdrasilRapidJSON" OFF)
  if(NOT YGGDRASIL_RAPIDJSON_INCLUDE_DIRS)
    find_package(YggdrasilRapidJSON)
    message(STATUS "YggdrasilRapidJSON_VERSION = ${YggdrasilRapidJSON_VERSION}")
  endif()
  if(YggdrasilRapidJSON_FOUND)
    yggdrasil_rapidjson_options_create()
  endif()
endmacro()

macro(ygginterface_target_config TARGET TYPE PREFIX)
  if(COMMAND yggdrasil_rapidjson_target_config)
    yggdrasil_rapidjson_target_config(${TARGET} ${TYPE} ${PREFIX})
  else()
    target_link_libraries(
      ${TARGET} ${TYPE} ${${PREFIX}_LIBRARIES}
      ${${PREFIX}_PUBLIC_LIBRARIES}
      "$<$<LINK_LANGUAGE:C>:${${PREFIX}_PUBLIC_C_LIBS}>"
      "$<$<LINK_LANGUAGE:CXX>:${${PREFIX}_PUBLIC_CXX_LIBS}>"
      "$<$<LINK_LANGUAGE:Fortran>:${${PREFIX}_PUBLIC_Fortran_LIBS}>"
    )
    target_include_directories(
      ${TARGET} ${TYPE} ${${PREFIX}_INCLUDE_DIRS}
    )
    target_compile_options(
      ${TARGET} ${TYPE} ${${PREFIX}_PUBLIC_COMPILE_FLAGS}
      "$<$<COMPILE_LANGUAGE:C>:${${PREFIX}_PUBLIC_C_COMPILE_FLAGS}>"
      "$<$<COMPILE_LANGUAGE:CXX>:${${PREFIX}_PUBLIC_CXX_COMPILE_FLAGS}>"
      "$<$<COMPILE_LANGUAGE:Fortran>:${${PREFIX}_PUBLIC_Fortran_COMPILE_FLAGS}>"
    )
    target_link_options(
      ${TARGET} ${TYPE} ${${PREFIX}_PUBLIC_LINK_FLAGS}
      "$<$<LINK_LANGUAGE:C>:${${PREFIX}_PUBLIC_C_LINK_FLAGS}>"
      "$<$<LINK_LANGUAGE:CXX>:${${PREFIX}_PUBLIC_CXX_LINK_FLAGS}>"
      "$<$<LINK_LANGUAGE:Fortran>:${${PREFIX}_PUBLIC_Fortran_LINK_FLAGS}>"
    )
    if(NOT TYPE STREQUAL "INTERFACE")
      target_link_libraries(
        ${TARGET} PRIVATE ${${PREFIX}_PRIVATE_LIBRARIES}
      )
      # target_include_directories(
      #   ${TARGET} PRIVATE ${${PREFIX}_PRIVATE_INCLUDE_DIRS}
      # )
      # target_compile_options(
      #   ${TARGET} PRIVATE ${${PREFIX}_PRIVATE_COMPILE_FLAGS}
      # )
      # target_link_options(
      #   ${TARGET} PRIVATE ${${PREFIX}_PRIVATE_LINK_FLAGS}
      # )
    endif()
  endif()
endmacro()

macro(ygginterface_options_config OUTPUT_PREFIX)
  foreach(flag BUILD_ASAN BUILD_UBSAN ENABLE_INSTRUMENTATION_OPT)
    set(YGGDRASIL_RAPIDJSON_${flag} ${YGG_${flag}})
  endforeach()
  set(YGGDRASIL_DISABLE_PYTHON_C_API ${YGGDRASIL_DISABLE_Python_C_API})
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
      APPEND ${OUTPUT_PREFIX}_PUBLIC_LIBRARIES
      YggdrasilRapidJSON
    )
    if((YGG_BUILD_ASAN OR YGG_BUILD_UBSAN)
       AND NOT COMMAND yggdrasil_rapidjson_target_config)
      # This can be removed after rapidjson updated
      list(REMOVE_ITEM ${OUTPUT_PREFIX}_PUBLIC_COMPILE_FLAGS
           ${${OUTPUT_PREFIX}_ASAN_COMPILE_FLAGS})
      list(REMOVE_ITEM ${OUTPUT_PREFIX}_PUBLIC_LINK_FLAGS
           ${${OUTPUT_PREFIX}_ASAN_COMPILE_FLAGS})
      # Compilation flags are same as link flags for ASAN & UBSAN
      foreach(suffix PUBLIC_C_COMPILE_FLAGS PUBLIC_CXX_COMPILE_FLAGS
              PUBLIC_C_LINK_FLAGS PUBLIC_CXX_LINK_FLAGS)
        list(
          APPEND ${OUTPUT_PREFIX}_${suffix}
          ${${OUTPUT_PREFIX}_ASAN_COMPILE_FLAGS}
        )
      endforeach()
      if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        execute_process(
          COMMAND ${CMAKE_CXX_COMPILER} -print-file-name=libclang_rt.asan_osx_dynamic.dylib
          WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
          OUTPUT_VARIABLE ${OUTPUT_PREFIX}_ASAN_LIB
          RESULT_VARIABLE ${OUTPUT_PREFIX}_ASAN_RESULT
        )
        if(${OUTPUT_PREFIX}_ASAN_RESULT)
          set(${OUTPUT_PREFIX}_ASAN_LIB)
        endif()
      endif()
    endif()
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
  if(YGG_ENABLE_COVERAGE AND
     CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
    find_program(GCOVR_FOUND gcovr)
    if (GCOVR_FOUND)
      message(STATUS "COVERAGE ON")
      set(COVERAGE_DIR ${CMAKE_BINARY_DIR}/coverage)
      file(MAKE_DIRECTORY ${COVERAGE_DIR})
      list(APPEND ${OUTPUT_PREFIX}_COVERAGE_COMPILE_FLAGS -g -O0 --coverage -fno-inline -fprofile-arcs -ftest-coverage)
      if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|ARMClang")
        set(CTEST_COVERAGE_COMMAND "llvm-cov")
        set(CTEST_COVERAGE_EXTRA_FLAGS "gcov")
        list(APPEND ${OUTPUT_PREFIX}_COVERAGE_COMPILE_FLAGS -fprofile-instr-generate -fcoverage-mapping)
      else()
        list(APPEND ${OUTPUT_PREFIX}_COVERAGE_LIBRARIES gcov)
        set(CTEST_COVERAGE_COMMAND "gcov")
        set(CTEST_COVERAGE_EXTRA_FLAGS "")
        list(APPEND ${OUTPUT_PREFIX}_COVERAGE_COMPILE_FLAGS -fno-inline-small-functions -fno-default-inline)
      endif()
    else()
      message(STATUS "Could not find gcovr, coverage disabled")
      set(YGG_ENABLE_COVERAGE OFF)
    endif()
  endif()
  if(${OUTPUT_PREFIX}_COVERAGE_COMPILE_FLAGS)
    # Compilation flags are same as link flags for coverage
    foreach(suffix PUBLIC_C_COMPILE_FLAGS PUBLIC_CXX_COMPILE_FLAGS
            PUBLIC_C_LINK_FLAGS PUBLIC_CXX_LINK_FLAGS)
      list(
        APPEND ${OUTPUT_PREFIX}_${suffix}
        ${${OUTPUT_PREFIX}_COVERAGE_COMPILE_FLAGS}
      )
    endforeach()
  endif()
  if(${OUTPUT_PREFIX}_COVERAGE_LIBRARIES)
    list(
      APPEND ${OUTPUT_PREFIX}_PRIVATE_LIBRARIES
      ${${OUTPUT_PREFIX}_COVERAGE_LIBRARIES}
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
  include(GeneralTools)
  split_string("${VERSION_STRING}" "." parts)
  list(GET parts 0 MAJOR_VERSION)
  list(GET parts 1 MINOR_VERSION)
  list(GET parts 2 PATCH_VERSION)
  configure_file(${SRC} ${DST} @ONLY)
endfunction()

##################
# Coverage       #
##################

function(cover_test target)
  if(YGG_ENABLE_COVERAGE AND
     CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang|AppleClang|ARMClang")
    add_test(NAME ${target}_coverage
             COMMAND sh -c "gcovr --root=${CMAKE_SOURCE_DIR} --gcov-executable=\"${CTEST_COVERAGE_COMMAND} ${CTEST_COVERAGE_EXTRA_FLAGS}\" --coveralls=${COVERAGE_DIR}/coverage.info --html-details=${COVERAGE_DIR}/coverage.html --exclude=tests/ --exclude=thirdparty/ --filter=${CMAKE_SOURCE_DIR} -v ${CMAKE_BINARY_DIR}"
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_tests_properties(${target}_coverage PROPERTIES DEPENDS "${target}")
  endif()
endfunction()
