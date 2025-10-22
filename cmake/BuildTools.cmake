include(GeneralTools)

function(add_custom_command_env target)
  set(options END_ENV)
  set(oneValueArgs COMMENT KEYPREFIX)
  set(multiValueArgs COMMAND ENV OUTPUT)  # DEPENDS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_ENV AND NOT ARGS_END_ENV)
    message(FATAL_ERROR "If ENV provided, END_ENV must be as well.")
  endif()
  # TODO: Replace use of execute_env.cmake.in
  setup_external_function(
    "execute_process_with_env" MODULE "GeneralTools"
    DEST ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake
    OUTPUT_COMMAND PROCESS_COMMAND
    ARGUMENTS COMMAND ${ARGS_COMMAND}
    ENV_VAR_PREFIX ${ARGS_KEYPREFIX}
    ENV_VARS ${ARGS_ENV} ENV_VARS_END
    COMMAND_ECHO STDOUT
  )
  # configure_file(
  #   ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/execute_env.cmake.in
  #   ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake
  #   @ONLY)
  if (ARGS_OUTPUT)
    add_custom_command(
      OUTPUT ${ARGS_OUTPUT}
      COMMAND ${CMAKE_COMMAND} ARGS -P ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake
      ${ARGS_UNPARSED_ARGUMENTS})
    add_custom_target(
      ${target} ALL
      DEPENDS ${ARGS_OUTPUT})
  else()
    add_custom_target(
      ${target} ALL
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/execute_env.cmake
      ${ARGS_UNPARSED_ARGUMENTS})
  endif()
endfunction()


function(get_runtime_environment_var OUTPUT_VARIABLE)
  if(WIN32)
    set(OUT PATH)
  elseif(APPLE)
    set(OUT DYLD_LIBRARY_PATH)
  else()
    set(OUT LD_LIBRARY_PATH)
  endif()
  set(${OUTPUT_VARIABLE} "${OUT}" PARENT_SCOPE)
endfunction()


function(get_pathsep OUTPUT_VARIABLE)
  set(oneValueArgs ESCAPE_LEVEL)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(WIN32)
    if(NOT ARGS_ESCAPE_LEVEL)
      set(ARGS_ESCAPE_LEVEL 0)
    endif()
    math(EXPR COUNT "${ARGS_ESCAPE_LEVEL}+1")
    string(REPEAT "\\" ${COUNT} ESCAPE_STR)
    set(OUT "${ESCAPE_STR};")
  else()
    set(OUT ":")
  endif()
  set(${OUTPUT_VARIABLE} "${OUT}" PARENT_SCOPE)
endfunction()


function(update_env_path)
  set(options PREPEND)
  set(oneValueArgs PATH_VARIABLE PATH_SEP OUTPUT_VARIABLE PREVIOUS_VALUE ESCAPE_LEVEL)
  set(multiValueArgs PATHS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_UNPARSED_ARGUMENTS)
    list(APPEND ARGS_PATHS ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if(NOT ARGS_PATHS)
    return()
  endif()
  if(NOT ARGS_ESCAPE_LEVEL)
    set(ARGS_ESCAPE_LEVEL 0)
  endif()
  if(NOT ARGS_PATH_VARIABLE)
    get_runtime_environment_var(ARGS_PATH_VARIABLE)
  endif()
  if(NOT ARGS_PATH_SEP)
    get_pathsep(ARGS_PATH_SEP ESCAPE_LEVEL ${ARGS_ESCAPE_LEVEL})
  endif()
  set(UPDATED_PATHS)
  if(ARGS_PREPEND)
    list(APPEND UPDATED_PATHS ${ARGS_PATHS})
  endif()
  if(DEFINED ENV{${ARGS_PATH_VARIABLE}})
    get_pathsep(path_sep_basic)
    string(REPLACE "${path_sep_basic}" ";" PREVIOUS_PATHS "$ENV{${ARGS_PATH_VARIABLE}}")
    list(LENGTH PREVIOUS_PATHS PREVIOUS_PATHS_LEN)
    message(DEBUG "PREVIOUS_PATHS = ${PREVIOUS_PATHS} [len = ${PREVIOUS_PATHS_LEN}]")
    list(APPEND UPDATED_PATHS ${PREVIOUS_PATHS})
  endif()
  if(NOT ARGS_PREPEND)
    list(APPEND UPDATED_PATHS ${ARGS_PATHS})
  endif()
  list(LENGTH UPDATED_PATHS UPDATED_PATHS_LEN)
  message(DEBUG "UPDATED_PATHS = ${UPDATED_PATHS} [len = ${UPDATED_PATHS_LEN}]")
  list(JOIN UPDATED_PATHS "${ARGS_PATH_SEP}" UPDATED_PATHS)
  list(LENGTH UPDATED_PATHS UPDATED_PATHS_LEN)
  message(DEBUG "UPDATED_PATHS = ${UPDATED_PATHS} [len = ${UPDATED_PATHS_LEN}]")
  if(WIN32)
    string(REPLACE "/" "\\" UPDATED_PATHS "${UPDATED_PATHS}")
  endif()
  if(ARGS_PREVIOUS_VALUE)
    set(${ARGS_PREVIOUS_VALUE} "$ENV{${ARGS_PATH_VARIABLE}}" PARENT_SCOPE)
  endif()
  if(ARGS_OUTPUT_VARIABLE)
    # PARENT_SCOPE?
    set(${ARGS_OUTPUT_VARIABLE} "${UPDATED_PATHS}")
    set(${ARGS_OUTPUT_VARIABLE} "${UPDATED_PATHS}" PARENT_SCOPE)
    message(DEBUG "update_env_path[VAR] ${ARGS_OUTPUT_VARIABLE} = ${${ARGS_OUTPUT_VARIABLE}}")
  else()
    set(ENV{${ARGS_PATH_VARIABLE}} "${UPDATED_PATHS}")
    message(DEBUG "update_env_path[ENV] ${ARGS_PATH_VARIABLE} = $ENV{${ARGS_PATH_VARIABLE}}")
  endif()
endfunction()


function(get_runtime_directory_suffix OUTPUT_VARIABLE)
  if(WIN32)
    set(OUT "bin")
  else()
    set(OUT "lib")
  endif()
  set(${OUTPUT_VARIABLE} "${OUT}" PARENT_SCOPE)
endfunction()


function(configure_and_build CONFIG_FILE)
  set(oneValueArgs SOURCE_DIR BUILD_DIR)
  set(multiValueArgs PREPEND_PATHS CMAKE_COMMAND_ARGS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_SOURCE_DIR)
    set(ARGS_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/_tmp)
  endif()
  if(NOT ARGS_BUILD_DIR)
    set(ARGS_BUILD_DIR ${ARGS_SOURCE_DIR}_build)
  endif()
  cmake_path(APPEND ARGS_SOURCE_DIR "CMakeLists.txt" OUTPUT_VARIABLE OUTPUT_FILE)
  configure_file(${CONFIG_FILE} ${OUTPUT_FILE} @ONLY)
  file(MAKE_DIRECTORY ${ARGS_BUILD_DIR})
  if(ARGS_PREPEND_PATHS)
    update_env_path(
      PREPEND
      PATH_VARIABLE PATH
      PATHS ${ARGS_PREPEND_PATHS}
      PREVIOUS_VALUE old_paths
    )
  endif()
  execute_process(
    COMMAND ${CMAKE_COMMAND} ${ARGS_SOURCE_DIR} ${ARGS_CMAKE_COMMAND_ARGS}
    WORKING_DIRECTORY ${ARGS_BUILD_DIR}
    COMMAND_ECHO STDOUT
    RESULT_VARIABLE ret)
  if (NOT ret EQUAL 0)
    message(FATAL_ERROR "Failed to configure ${OUTPUT_FILE}")
  endif()
  execute_process(
    COMMAND ${CMAKE_COMMAND} --build . --config Release
    WORKING_DIRECTORY ${ARGS_BUILD_DIR}
    COMMAND_ECHO STDOUT
    RESULT_VARIABLE ret)
  if(old_paths)
    set(ENV{PATH} "${old_paths}")
  endif()
  if (NOT ret EQUAL 0)
    message(FATAL_ERROR "Failed to build ${OUTPUT_FILE}")
  endif()
endfunction()


function(configure_env_injection)
  set(oneValueArgs OUTPUT_FILE DIRECTORY)
  set(multiValueArgs VARIABLES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_OUTPUT_FILE)
    set(ARGS_OUTPUT_FILE ${CMAKE_CURRENT_BINARY_DIR}/CTestEnvInject.cmake)
  endif()
  message(DEBUG "configure_env_injection1 ARGS_VARIABLES = ${ARGS_VARIABLES}")
  if (ARGS_UNPARSED_ARGUMENTS)
    list(APPEND ARGS_VARIABLES ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  message(DEBUG "configure_env_injection2 ARGS_VARIABLES = ${ARGS_VARIABLES}")
  string(REPLACE "\\" "\\\\\\\\" ENV_VARS "${ARGS_VARIABLES}")
  message(DEBUG "configure_env_injection ENV_VARS = ${ENV_VARS}")
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/CTestEnvInject.cmake.in
    ${ARGS_OUTPUT_FILE}
    @ONLY)
  file(READ ${ARGS_OUTPUT_FILE} CONTENTS)
  message(DEBUG "CONTENTS = ${CONTENTS}")
  if (ARGS_DIRECTORY)
    set_property(
      DIRECTORY ${ARGS_DIRECTORY} APPEND PROPERTY
      TEST_INCLUDE_FILES ${ARGS_OUTPUT_FILE}
    )
  endif()
endfunction()

function(configure_path_injection)
  set(oneValueArgs OUTPUT_FILE PATH_VARIABLE DIRECTORY)
  set(multiValueArgs PATHS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_OUTPUT_FILE)
    set(ARGS_OUTPUT_FILE ${CMAKE_CURRENT_BINARY_DIR}/CTestPathInject.cmake)
  endif()
  if (NOT ARGS_PATH_VARIABLE)
    set(ARGS_PATH_VARIABLE PATH)
  endif()
  set(PATH_VAR ${ARGS_PATH_VARIABLE})
  if(ARGS_UNPARSED_ARGUMENTS)
    list(APPEND ARGS_PATHS ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if(ARGS_PATHS)
    get_pathsep(PATH_SEP ESCAPE_LEVEL 2)
    list(JOIN ARGS_PATHS "${PATH_SEP}" NEW_PATHS)
  else()
    set(PATH_SEP)
    set(NEW_PATHS)
  endif()
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/CTestPathInject.cmake.in
    ${ARGS_OUTPUT_FILE}
    @ONLY)
  if (ARGS_DIRECTORY)
    set_property(
      DIRECTORY ${ARGS_DIRECTORY} APPEND PROPERTY
      TEST_INCLUDE_FILES ${ARGS_OUTPUT_FILE}
    )
  endif()
endfunction()


function(cmakevar2cmakecliarg opt OUTPUT_LIST_VARIABLE)
  if(NOT ${opt})
    return()
  endif()
  list(LENGTH ${opt} opt_len)
  if (opt_len GREATER 1)
    list(JOIN ${opt} "\\\\;" tmp)
    set(OUT -D${opt}=${tmp})
  else()
    set(OUT -D${opt}=${${opt}})
  endif()
  list(APPEND ${OUTPUT_LIST_VARIABLE} ${OUT})
  set(${OUTPUT_LIST_VARIABLE} "${${OUTPUT_LIST_VARIABLE}}" PARENT_SCOPE)
endfunction()


function(cmakevars2cmakecliargs OUTPUT_LIST_VARIABLE)
  foreach(opt ${ARGN})
    cmakevar2cmakecliarg(${opt} ${OUTPUT_LIST_VARIABLE})
  endforeach()
  set(${OUTPUT_LIST_VARIABLE} "${${OUTPUT_LIST_VARIABLE}}" PARENT_SCOPE)
endfunction()

function(python_code_generation NAME SCRIPT)
  # TODO: Conditional generation on sources
  set(oneValueArgs WORKING_DIRECTORY)
  set(multiValueArgs ARGUMENTS SOURCES BYPRODUCTS ENTRY_POINT)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(IS_MODULE OFF)
  cmake_path(ABSOLUTE_PATH SCRIPT NORMALIZE)
  if(IS_DIRECTORY ${SCRIPT})
    set(IS_MODULE ON)
  endif()
  if(NOT ARGS_ENTRY_POINT)
    if(IS_MODULE)
      set(ARGS_ENTRY_POINT -m ${SCRIPT})
    else()
      set(ARGS_ENTRY_POINT ${SCRIPT})
    endif()
  endif()
  if(IS_MODULE)
    file(GLOB_RECURSE MODULE_SOURCES "${SCRIPT}/*.py")
    list(APPEND ARGS_SOURCES ${MODULE_SOURCES})
    if(NOT ARGS_WORKING_DIRECTORY)
      set(ARGS_WORKING_DIRECTORY ${SCRIPT})
    endif()
  endif()
  if (ARGS_WORKING_DIRECTORY)
    cmake_path(ABSOLUTE_PATH ARGS_WORKING_DIRECTORY NORMALIZE)
    list(APPEND ARGS_UNPARSED_ARGUMENTS
         WORKING_DIRECTORY ${ARGS_WORKING_DIRECTORY})
  endif()
  if (ARGS_SOURCES)
    list(APPEND ARGS_UNPARSED_ARGUMENTS DEPENDS)  # SOURCES)
    foreach(src ${ARGS_SOURCES})
      cmake_path(ABSOLUTE_PATH src NORMALIZE)
      list(APPEND ARGS_UNPARSED_ARGUMENTS ${src})
    endforeach()
  endif()
  set(ARGS_UNPARSED_ARGUMENTS_CMD ${ARGS_UNPARSED_ARGUMENTS})
  if (ARGS_BYPRODUCTS)
    list(APPEND ARGS_UNPARSED_ARGUMENTS BYPRODUCTS)
    list(APPEND ARGS_UNPARSED_ARGUMENTS_CMD OUTPUT)
    foreach(src ${ARGS_BYPRODUCTS})
      cmake_path(ABSOLUTE_PATH src NORMALIZE)
      list(APPEND ARGS_UNPARSED_ARGUMENTS ${src})
      list(APPEND ARGS_UNPARSED_ARGUMENTS_CMD ${src})
    endforeach()
  endif()
  if (${Python_PREFIX}_EXECUTABLE)
    message(STATUS "Calling python script ${SCRIPT}")
    if (ARGS_BYPRODUCTS)
      add_custom_command(
        COMMAND ${${Python_PREFIX}_EXECUTABLE} ${ARGS_ENTRY_POINT}
        ${ARGS_ARGUMENTS}
        ${ARGS_UNPARSED_ARGUMENTS_CMD}
      )
      add_custom_target(${NAME} ALL DEPENDS ${ARGS_BYPRODUCTS})
    else()
      add_custom_target(
        ${NAME}
        COMMAND ${${Python_PREFIX}_EXECUTABLE} ${ARGS_ENTRY_POINT}
        ${ARGS_ARGUMENTS}
        ${ARGS_UNPARSED_ARGUMENTS}
      )
    endif()
  endif()
endfunction()

function(predict_target_filename target library_type language output_var)
  set(oneValueArgs BUILD_DIR)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_BUILD_DIR)
    set(ARGS_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}")
  endif()
  set(LIBRARY_PREFIX "${CMAKE_${library_type}_LIBRARY_PREFIX_${language}}")
  set(LIBRARY_SUFFIX "${CMAKE_${library_type}_LIBRARY_SUFFIX_${language}}")
  if(NOT LIBRARY_PREFIX)
    set(LIBRARY_PREFIX "${CMAKE_${library_type}_LIBRARY_PREFIX}")
  endif()
  if(NOT LIBRARY_SUFFIX)
    set(LIBRARY_SUFFIX "${CMAKE_${library_type}_LIBRARY_SUFFIX}")
  endif()
  cmake_path(
    APPEND OUTPUT "${ARGS_BUILD_DIR}"
    "${LIBRARY_PREFIX}${target}${LIBRARY_SUFFIX}"
  )
  set(${output_var} "${OUTPUT}" PARENT_SCOPE)
endfunction()

function(get_native_directory PATH OUTPUT_VAR)
  if(NOT PATH)
    message(FATAL_ERROR "Empty path provided")
  endif()
  get_filename_component(DIRECTORY ${PATH} PATH)
  file(TO_NATIVE_PATH "${DIRECTORY}" DIRECTORY)
  string(REPLACE "\\" "\\\\" DIRECTORY "${DIRECTORY}")
  set(${VAR} ${DIRECTORY} PARENT_SCOPE)
endfunction()

function(find_compiler_external language)
  set(options REQUIRED SKIP_CURRENT_GENERATOR DONT_CLEAR_OTHER_COMPILERS
      OVERWRITE)
  set(oneValueArgs GENERATOR TIMEOUT LINKER_LANGUAGE ID
      OUTPUT_COMPILER OUTPUT_LINKER OUTPUT_GENERATOR)
  set(multiValueArgs TRY_GENERATORS CLEAR_COMPILERS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(EXTERNAL_VARIABLES COMPILER LINKER GENERATOR)
  if(ARGS_LINKER_LANGUAGE AND (ARGS_LINKER_LANGUAGE STREQUAL "${language}"))
    set(ARGS_LINKER_LANGUAGE)
  endif()
  if(NOT ARGS_ID)
    if(ARGS_LINKER_LANGUAGE)
      set(ARGS_ID "${language}_${ARGS_LINKER_LANGUAGE}_EXTERNAL")
    else()
      set(ARGS_ID "${language}_EXTERNAL")
    endif()
  endif()
  if((NOT ARGS_OVERWRITE) AND
     CMAKE_${ARGS_ID}_COMPILER AND
     ((NOT ARGS_LINKER_LANGUAGE) OR CMAKE_${ARGS_ID}_LINKER))
    foreach(ivar ${EXTERNAL_VARIABLES})
      if(ARGS_OUTPUT_${ivar})
        set(${ARGS_OUTPUT_${ivar}} "${CMAKE_${ARGS_ID}_${ivar}}" PARENT_SCOPE)
      endif()
    endforeach()
    return()
  endif()
  if(ARGS_OVERWRITE)
    foreach(ivar ${EXTERNAL_VARIABLES)
      set(CMAKE_${ARGS_ID}_${ivar})
    endforeach()
  endif()
  include(GeneralTools)
  if(NOT (ARGS_DONT_CLEAR_OTHER_COMPILERS OR ARGS_CLEAR_COMPILERS))
    get_supported_languages(
      ARGS_CLEAR_COMPILERS COMPILED
      EXCLUDE ${language} ${ARGS_CLEAR_COMPILERS}
    )
  endif()
  set(ARGS_DONT_CLEAR_OTHER_COMPILERS ON)
  if(language STREQUAL "Fortran" AND
     ARGS_GENERATOR MATCHES "Visual Studio" AND
     NOT ARGS_TRY_GENERATORS)
    set_default(ARGS_TIMEOUT 30)
  endif()
  collect_arguments(
    PROCESS_ARGS ARGS "${options}"
    TIMEOUT CLEAR_COMPILERS
  )
  if(ARGS_TRY_GENERATORS)
    collect_arguments(
      CHILD_ARGS ARGS "${options}"
      LINKER_LANGUAGE ID DONT_CLEAR_OTHER_COMPILERS
    )
    if(ARGS_TRY_GENERATORS STREQUAL "ALL")
      get_supported_generators(ARGS_TRY_GENERATORS)
    endif()
    foreach(igen IN LISTS ARGS_TRY_GENERATORS)
      if(ARGS_SKIP_CURRENT_GENERATOR
         AND igen STREQUAL "${CMAKE_GENERATOR}")
        continue()
      endif()
      message(DEBUG "Trying generator \"${igen}\"")
      find_compiler_external(
        ${language} GENERATOR "${igen}"
        ${CHILD_ARGS} ${PROCESS_ARGS} ${ARGS_UNPARSED_ARGUMENTS}
      )
      if(CMAKE_${ARGS_ID}_COMPILER)
        foreach(ivar ${EXTERNAL_VARIABLES})
          set(CMAKE_${ARGS_ID}_${ivar} "${CMAKE_${ARGS_ID}_${ivar}}" PARENT_SCOPE)
          if(ARGS_OUTPUT_${ivar})
            set(${ARGS_OUTPUT_${ivar}} "${CMAKE_${ARGS_ID}_${ivar}}" PARENT_SCOPE)
          endif()
        endforeach()
        return()
      endif()
    endforeach()
    if(WIN32 AND language STREQUAL "Fortran" AND
       (NOT CMAKE_${ARGS_ID}_COMPILER))
      find_mingw_gfortran(LINKER_LANGUAGE ${ARGS_LINKER_LANGUAGE})
      if(MINGW_GFORTRAN)
        set(CMAKE_${ARGS_ID}_COMPILER "${MINGW_GFORTRAN}" PARENT_SCOPE)
        set(CMAKE_${ARGS_ID}_LINKER "" PARENT_SCOPE)  # TODO
        set(CMAKE_${ARGS_ID}_GENERATOR "MinGW Makefiles" PARENT_SCOPE)
        return()
      endif()
    endif()
    if(ARGS_REQUIRED AND NOT CMAKE_${ARGS_ID}_COMPILER)
      message(FATAL_ERROR "Failed to locate a ${language} compiler (Tried generators: ${ARGS_TRY_GENERATORS})")
    endif()
    return()
  endif()
  if(NOT ARGS_GENERATOR)
    set(ARGS_GENERATOR "${CMAKE_GENERATOR}")
  endif()
  if(language STREQUAL "Fortran" AND ARGS_GENERATOR MATCHES "Visual Studio")
    set_default(ARGS_TIMEOUT 30)
  endif()
  string(REPLACE " " "_" GENSTR "${ARGS_GENERATOR}")
  set(IDSTR "${ARGS_ID}_${GENSTR}")
  set(cached_compiler_var "_${IDSTR}_COMPILER")
  set(cached_linker_var "_${IDSTR}_LINKER")
  if(NOT DEFINED CACHE{${cached_compiler_var}})
    set(tmp_dir "${CMAKE_CURRENT_BINARY_DIR}/_check_for_${IDSTR}")
    message(DEBUG "find_compiler_external(${language} GENERATOR ${ARGS_GENERATOR}): tmp_dir = ${tmp_dir}")
    set(fcompiler "${tmp_dir}/${IDSTR}_compiler")
    set(flinker "${tmp_dir}/${IDSTR}_linker")
    file(MAKE_DIRECTORY "${tmp_dir}")
    configure_file(
      ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/check_language_external.CMakeLists.in
      ${tmp_dir}/CMakeLists.txt
      @ONLY)
    message(STATUS "Looking for ${language} compiler using \"${ARGS_GENERATOR}\" generator")
    execute_process_with_env(
      COMMAND ${CMAKE_COMMAND} "-G${ARGS_GENERATOR}" -B . -S .
      WORKING_DIRECTORY ${tmp_dir}
      RESULT_VARIABLE out
      ${PROCESS_ARGS}
      ${ARGS_UNPARSED_ARGUMENTS}
    )
    if (EXISTS ${fcompiler})
      file(READ ${fcompiler} ${cached_compiler_var})
      if(EXISTS ${flinker})
        file(READ ${flinker} ${cached_linker_var})
      endif()
    endif()
    set(${cached_compiler_var} "${${cached_compiler_var}}" CACHE INTERNAL "Compiler result from find_compiler_external for LANGUAGE = ${language}, LINKER_LANGUAGE = ${ARGS_LINKER_LANGUAGE}, and GENERATOR = ${ARGS_GENERATOR}")
    set(${cached_linker_var} "${${cached_linker_var}}" CACHE INTERNAL "Linker result from find_compiler_external for LANGUAGE = ${language}, LINKER_LANGUAGE = ${ARGS_LINKER_LANGUAGE}, and GENERATOR = ${ARGS_GENERATOR}")
  endif()
  if(${cached_compiler_var})
    set(CMAKE_${ARGS_ID}_COMPILER ${${cached_compiler_var}})
    set(CMAKE_${ARGS_ID}_LINKER ${${cached_linker_var}})
    set(CMAKE_${ARGS_ID}_GENERATOR "${ARGS_GENERATOR}")
    message(STATUS "Located external compiler for ${language} (GENERATOR=${ARGS_GENERATOR}, LINKER_LANGUAGE=${ARGS_LINKER_LANGUAGE})")
    foreach(ivar ${EXTERNAL_VARIABLES})
      message(STATUS "CMAKE_${ARGS_ID}_${ivar} = ${CMAKE_${ARGS_ID}_${ivar}}")
    endforeach()
  elseif(ARGS_REQUIRED)
    message(FATAL_ERROR "Failed to locate a ${language} external compiler (CMAKE_GENERATOR = \"${ARGS_GENERATOR}\")")
  endif()
  foreach(ivar ${EXTERNAL_VARIABLES})
    set(CMAKE_${ARGS_ID}_${ivar} "${CMAKE_${ARGS_ID}_${ivar}}" PARENT_SCOPE)
    if(ARGS_OUTPUT_${ivar})
      set(${ARGS_OUTPUT_${ivar}} "${CMAKE_${ARGS_ID}_${ivar}}" PARENT_SCOPE)
    endif()
  endforeach()
endfunction()

function(check_language_external language)
  set(options REQUIRED SKIP_CURRENT_GENERATOR DONT_CLEAR_OTHER_COMPILERS
      OVERWRITE OUTPUT_GENERATOR)
  # Ensure uses replace GENERATOR with OUTPUT_GENERATOR
  set(oneValueArgs TIMEOUT LINKER_LANGUAGE ID OUTPUT_VARIABLE)
  # set(oneValueArgs GENERATOR TIMEOUT LINKER_LANGUAGE ID)
  set(multiValueArgs TRY_GENERATORS CLEAR_COMPILERS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  check_no_unparsed(ARGS)
  if(NOT (ARGS_GENERATOR OR ARGS_TRY_GENERATORS))
    set(ARGS_TRY_GENERATORS ALL)
  endif()
  if(ARGS_LINKER_LANGUAGE AND (ARGS_LINKER_LANGUAGE STREQUAL "${language}"))
    set(ARGS_LINKER_LANGUAGE)
  endif()
  if(NOT ARGS_ID)
    if(ARGS_LINKER_LANGUAGE)
      set(ARGS_ID "${language}_${ARGS_LINKER_LANGUAGE}_EXTERNAL")
    else()
      set(ARGS_ID "${language}_EXTERNAL")
    endif()
  endif()
  collect_arguments(
    FIND_ARGS ARGS "${options}"
    REQUIRED SKIP_CURRENT_GENERATOR DONT_CLEAR_OTHER_COMPILERS
    OVERWRITE GENERATOR TIMEOUT LINKER_LANGUAGE ID
    TRY_GENERATORS CLEAR_COMPILERS
  )
  find_compiler_external(
    ${language} ${FIND_ARGS}
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  set(CMAKE_${language}_COMPILER ${CMAKE_${ARGS_ID}_COMPILER} PARENT_SCOPE)
  set(CMAKE_${ARGS_ID}_COMPILER ${CMAKE_${ARGS_ID}_COMPILER} PARENT_SCOPE)
  set(CMAKE_${ARGS_ID}_GENERATOR ${CMAKE_${ARGS_ID}_GENERATOR} PARENT_SCOPE)
  if(CMAKE_${ARGS_ID}_COMPILER AND ARGS_OUTPUT_VARIABLE)
    set(${ARGS_OUTPUT_VARIABLE} ON PARENT_SCOPE)
  endif()
  if(CMAKE_${ARGS_ID}_GENERATOR AND ARGS_OUTPUT_GENERATOR)
    set(${ARGS_OUTPUT_GENERATOR} "${CMAKE_${ARGS_ID}_GENERATOR}" PARENT_SCOPE)
  endif()
endfunction()

function(find_mingw_gfortran)
  set(options REQUIRED)
  set(oneValueArgs LINKER_LANGUAGE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  find_program(MINGW_GFORTRAN
    NAMES gfortran
    PATHS
      c:/MinGW/bin
      "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\MinGW;InstallLocation]/bin"
    )
  if(NOT MINGW_GFORTRAN)
    if (ARGS_REQUIRED)
      message(FATAL_ERROR
        "gfortran not found, please install MinGW with the gfortran option."
        "Or set the cache variable MINGW_GFORTRAN to the full path. "
        " This is required to build")
    endif()
    return()
  endif()

  # Validate the MinGW gfortran we found.
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(_mingw_target "Target:.*64.*mingw")
  else()
    set(_mingw_target "Target:.*mingw32")
  endif()
  execute_process(COMMAND "${MINGW_GFORTRAN}" -v
    ERROR_VARIABLE out ERROR_STRIP_TRAILING_WHITESPACE)
  if(NOT "${out}" MATCHES "${_mingw_target}")
    string(REPLACE "\n" "\n  " out "  ${out}")
    if (ARGS_REQUIRED)
      message(FATAL_ERROR
        "MINGW_GFORTRAN is set to\n"
        "  ${MINGW_GFORTRAN}\n"
        "which is not a MinGW gfortran for this architecture.  "
        "The output from -v does not match \"${_mingw_target}\":\n"
        "${out}\n"
        "Set MINGW_GFORTRAN to a proper MinGW gfortran for this architecture."
      )
    endif()
  endif()
  set(MINGW_GFORTRAN ${MINGW_GFORTRAN} PARENT_SCOPE)
endfunction()

function(setup_external_config lists_dir)
  set(options DONT_SET_COMMAND)
  set(oneValueArgs GENERATOR BUILD_DIR SOURCE_DIR FILENAME)
  set(multiValueArgs ARGUMENTS PRESERVE_VARIABLES CLEAR_COMPILERS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_GENERATOR)
    set(ARGS_GENERATOR "${CMAKE_GENERATOR}")
  endif()
  if(NOT ARGS_SOURCE_DIR)
    set(ARGS_SOURCE_DIR "${lists_dir}/src")
  endif()
  if(NOT ARGS_BUILD_DIR)
    set(ARGS_BUILD_DIR "${lists_dir}")
  endif()
  if(NOT ARGS_FILENAME)
    set(ARGS_FILENAME "external_config.cmake")
  endif()
  if(ARGS_CLEAR_COMPILERS)
    list(APPEND ARGS_UNPARSED_ARGUMENTS CLEAR_COMPILERS ${ARGS_CLEAR_COMPILERS})
  endif()
  list(APPEND ARGS_PRESERVE_VARIABLES
       CMAKE_VERBOSE_MAKEFILE CMAKE_MESSAGE_LOG_LEVEL)
  foreach(language C CXX Fortran)
    list(APPEND ARGS_PRESERVE_VARIABLES "CMAKE_${language}_OUTPUT_EXTENSION")
  endforeach()
  foreach(var IN LISTS ARGS_PRESERVE_VARIABLES)
    list(APPEND ARGS_ARGUMENTS "-D${var}=${${var}}")
  endforeach()
  set(EXTERNAL_COMMAND "${CMAKE_COMMAND}"
      "-G${ARGS_GENERATOR}" "-S${ARGS_SOURCE_DIR}" "-B${ARGS_BUILD_DIR}"
      ${ARGS_ARGUMENTS})
  setup_external_function(
    "execute_process_with_env" MODULE "GeneralTools"
    DEST ${lists_dir}/${ARGS_FILENAME}
    OUTPUT_COMMAND CONFIGURE_COMMAND
    ARGUMENTS COMMAND ${EXTERNAL_COMMAND}
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  if(NOT ARGS_DONT_SET_COMMAND)
    set(CONFIGURE_COMMAND ${CONFIGURE_COMMAND} PARENT_SCOPE)
  endif()
endfunction()

function(setup_external_build build_dir)
  set(options DONT_SET_COMMAND)
  set(oneValueArgs FILENAME)
  set(multiValueArgs ARGUMENTS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_FILENAME)
    set(ARGS_FILENAME "external_build.cmake")
  endif()
  setup_external_function(
    "execute_process_with_env" MODULE "GeneralTools"
    DEST ${build_dir}/${ARGS_FILENAME}
    OUTPUT_COMMAND BUILD_COMMAND
    ARGUMENTS COMMAND "${CMAKE_COMMAND}" "--build" "."
    ${ARGS_ARGUMENTS} ${ARGS_UNPARSED_ARGUMENTS}
  )
  if(NOT ARGS_DONT_SET_COMMAND)
    set(BUILD_COMMAND ${BUILD_COMMAND} PARENT_SCOPE)
  endif()
endfunction()

function(check_language_compat base_language language VAR)
  if(base_language STREQUAL "${language}")
    set(${VAR} ON PARENT_SCOPE)
    return()
  endif()
  if((base_language STREQUAL "C" AND language STREQUAL "CXX") OR
     (base_language STREQUAL "CXX" AND language STREQUAL "C"))
    set(${VAR} ON PARENT_SCOPE)
    return()
  endif()
  include(CheckLanguage)
  check_language(${language})
  if(CMAKE_${language}_COMPILER)
    set(${VAR} ON PARENT_SCOPE)
  endif()
  set(${VAR} OFF PARENT_SCOPE)
endfunction()

function(add_mixed_language_library target library_type)
  include(GeneralTools)
  set(options FORCE_EXTERNAL)
  set(oneValueArgs LINKER_LANGUAGE BASE_LANGUAGE)
  set(multiValueArgs LANGUAGES SOURCES LIBRARIES INCLUDES DEFINITIONS
      PROPERTIES COMPILE_FLAGS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_SOURCES)
    set(ARGS_SOURCES ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  message(DEBUG "add_mixed_language_library[${target}]: SOURCES = ${ARGS_SOURCES}")
  sort_files_by_language(
    SRC SOURCES ${ARGS_SOURCES}
    OUTPUT_LANGUAGES SOURCE_LANGUAGES
  )
  if(NOT ARGS_LANGUAGES)
    set(ARGS_LANGUAGES ${SOURCE_LANGUAGES})
  endif()
  list(FIND ARGS_LANGUAGES "Fortran" Fortran_IDX)
  list(FIND ARGS_LANGUAGES "CXX" CXX_IDX)
  list(FIND ARGS_LANGUAGES "C" C_IDX)
  if((MSVC OR FORCE_SPLIT_CXXFortran) AND (NOT Fortran_IDX EQUAL -1))
    set(ARGS_FORCE_EXTERNAL ON)
  endif()

  if((NOT ARGS_LINKER_LANGUAGE) AND (NOT CXX_IDX EQUAL -1))
    set(ARGS_LINKER_LANGUAGE CXX)
  endif()
  if(NOT ARGS_BASE_LANGUAGE)
    list(LENGTH ARGS_LANGUAGES LANGUAGE_COUNT)
    if(LANGUAGE_COUNT EQUAL 1)
      if(ARGS_FORCE_EXTERNAL)
        set(ARGS_BASE_LANGUAGE C)
      else()
        set(ARGS_BASE_LANGUAGE ${ARGS_LANGUAGES})
      endif()
    else()
      if(NOT C_IDX EQUAL -1)
        set(ARGS_BASE_LANGUAGE C)
      else()
        list(GET ARGS_LANGUAGES 0 ARGS_BASE_LANGUAGE)
      endif()
    endif()
  endif()

  list(FIND ARGS_LANGUAGES ${ARGS_BASE_LANGUAGE} BASE_LANGUAGE_IDX)
  if(BASE_LANGUAGE_IDX EQUAL -1)
    list(APPEND ARGS_LANGUAGES ${ARGS_BASE_LANGUAGE})
  endif()
  message(DEBUG "add_mixed_language_library[${target}]: LANGUAGES = ${ARGS_LANGUAGES}")
  
  foreach(ilanguage IN LISTS ARGS_LANGUAGES)
    if(NOT SRC_${ilanguage})
      # Add dummy source files for missing languages
      set(iext)
      language2srcext(${ilanguage} iext)
      set(dummy_src "${CMAKE_CURRENT_BINARY_DIR}/${target}_dummy${iext}")
      configure_file(
        ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/dummy${iext}.in
        ${dummy_src}
        @ONLY
      )
      list(APPEND SRC_${ilanguage} ${dummy_src})
    endif()
    message(DEBUG "add_mixed_language_library[${target}]: SOURCES_${ilanguage} = ${SRC_${ilanguage}}")
  endforeach()

  # Base library that external libraries or sources will be added to
  # from the other languages
  add_internal_library(
    ${target} ${library_type}
    LANGUAGE ${ARGS_BASE_LANGUAGE}
    SOURCES ${SRC_${ARGS_BASE_LANGUAGE}}
    LIBRARIES ${ARGS_LIBRARIES}
    INCLUDES ${ARGS_INCLUDES}
    DEFINITIONS ${ARGS_DEFINITIONS}
    COMPILE_FLAGS ${ARGS_COMPILE_FLAGS}
    PROPERTIES ${ARGS_PROPERTIES}
    LINKER_LANGUAGE ${ARGS_LINKER_LANGUAGE}
  )

  foreach(ilanguage IN LISTS ARGS_LANGUAGES)
    if(ilanguage STREQUAL "${ARGS_BASE_LANGUAGE}")
      continue()
    endif()
  
    set(${ilanguage}_target "${target}_${ilanguage}_OLIB")
    if(ARGS_FORCE_EXTERNAL)
      set(${ilanguage}_external ON)
    else()
      check_language_compat(
        ${ARGS_BASE_LANGUAGE} ${ilanguage} ${ilanguage}_external
      )
    endif()
    if(${ilanguage}_external)
      set(CMAKE_${ilanguage}_OUTPUT_EXTENSION ${CMAKE_${ARGS_BASE_LANGUAGE}_OUTPUT_EXTENSION})
      add_external_library(
        ${${ilanguage}_target} OBJECT LANGUAGE ${ilanguage}
        SOURCES ${SRC_${ilanguage}}
        LIBRARIES ${ARGS_LIBRARIES}
        INCLUDES ${ARGS_INCLUDES}
        DEFINITIONS ${ARGS_DEFINITIONS}
        COMPILE_FLAGS ${ARGS_COMPILE_FLAGS}
        PROPERTIES ${ARGS_PROPERTIES}
        LINKER_LANGUAGE ${ARGS_LINKER_LANGUAGE}
        PARENT_TARGET ${target}
      )
      copy_target_files(
        ${${ilanguage}_target} ${CMAKE_CURRENT_BINARY_DIR}
        EVENT_TARGET ${target} EVENT_TYPE PRE_LINK
        TARGET_TYPE OBJECT
        TARGET_LANGUAGE ${ilanguage}
        EXCLUDE_COMPONENTS OBJECTS
      )
      target_link_libraries(
        ${target} PRIVATE
        $<TARGET_PROPERTY:${${ilanguage}_target},INTERFACE_LINK_LIBRARIES>
      )
      target_link_directories(
        ${target} PRIVATE
        $<TARGET_PROPERTY:${${ilanguage}_target},INTERFACE_LINK_DIRECTORIES>
      )
      target_sources(
        ${target} PRIVATE "$<TARGET_OBJECTS:${${ilanguage}_target}>"
      )
      if(WIN32 AND MSVC)
        target_link_options(
          ${target} PRIVATE
          /DEF:$<TARGET_PROPERTY:${${ilanguage}_target},MSVC_DEF_FILE>
        )
      endif()
    else()
      enable_language(${ilanguage})
      if(ilanguage STREQUAL "Fortran")
        include(FortranCInterface)
        FortranCInterface_VERIFY()
        FortranCInterface_VERIFY(CXX)
        set_source_files_properties(
          ${SRC_${ilanguage}}
          PROPERTIES
          COMPILE_FLAGS "-cpp -fPIC"
          Fortran_STANDARD 2003
          Fortran_STANDARD_REQUIRED ON
          Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
      endif()
      target_sources(${target} PRIVATE ${SRC_${ilanguage}})
    endif()
  endforeach()
endfunction()

macro(parse_properties)
  set(propertyValueArgs Fortran_MODULE_DIRECTORY
      Fortran_STANDARD Fortran_STANDARD_REQUIRED)
  cmake_parse_arguments(PROPS "" "${propertyValueArgs}" "" ${ARGS_PROPERTIES})
endmacro()

macro(set_default_property name value)
  if(NOT PROPS_${name})
    set(PROPS_${name} "${value}")
    list(APPEND ARGS_PROPERTIES ${name} "${PROPS_${name}}")
  endif()
endmacro()

function(add_internal_library target library_type)
  set(oneValueArgs LANGUAGE LINKER_LANGUAGE TARGETS_FILE)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES DEFINITIONS PROPERTIES
      COMPILE_FLAGS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  parse_properties()
  if(NOT ARGS_SOURCES)
    set(ARGS_SOURCES ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if(ARGS_LANGUAGE STREQUAL "Fortran")
    list(APPEND ARGS_COMPILE_FLAGS -fPIC -cpp)
    set_default_property(Fortran_STANDARD 2003)
    set_default_property(Fortran_STANDARD_REQUIRED ON)
    set_default_property(Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
  endif()
  message(DEBUG "${target}: ${ARGS_LANGUAGE} ${library_type}")
  message(DEBUG "${target}: SOURCES = ${ARGS_SOURCES}")
  if(ARGS_LANGUAGE)
    enable_language(${ARGS_LANGUAGE})
  endif()
  if(ARGS_LINKER_LANGUAGE AND
     (NOT ARGS_LANGUAGE STREQUAL "${ARGS_LINKER_LANGUAGE}"))
    enable_language(${ARGS_LINKER_LANGUAGE})
  endif()
  add_library(${target} ${library_type} ${ARGS_SOURCES})
  if(ARGS_LINKER_LANGUAGE)
    message(DEBUG "${target}: LINKER_LANGUAGE = ${ARGS_LINKER_LANGUAGE}")
    set_target_properties(
      ${target} PROPERTIES LINKER_LANGUAGE ${ARGS_LINKER_LANGUAGE}
    )
  endif()
  if(ARGS_COMPILE_FLAGS)
    message(DEBUG "${target}: COMPILE_FLAGS = ${ARGS_COMPILE_FLAGS}")
    string(REPLACE ";" " " compile_flags_str "${ARGS_COMPILE_FLAGS}")
    set_target_properties(${target} PROPERTIES COMPILE_FLAGS "${compile_flags_str}")
  endif()
  if (library_type STREQUAL "SHARED" AND CMAKE_GNUtoMS)
    set_target_properties(${target} PROPERTIES IMPORT_PREFIX "" PREFIX "")
  endif()
  if(WIN32)
    set_target_properties(
      ${target} PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON
    )
  endif()
  if(ARGS_PROPERTIES)
    message(DEBUG "${target}: PROPERTIES = ${ARGS_PROPERTIES}")
    set_target_properties(${target} PROPERTIES ${ARGS_PROPERTIES})
  endif()
  if(ARGS_TARGETS_FILE)
    message(DEBUG "${target}: TARGETS_FILE = ${ARGS_TARGETS_FILE}")
    include(AddTargetsFromFile)
    target_link_from_file(${target} PUBLIC ${ARGS_TARGETS_FILE})
  endif()
  if(ARGS_LIBRARIES)
    message(DEBUG "${target}: LIBRARIES = ${ARGS_LIBRARIES}")
    target_link_libraries(${target} PUBLIC ${ARGS_LIBRARIES})
  endif()
  if(ARGS_INCLUDES)
    message(DEBUG "${target}: INCLUDES = ${ARGS_INCLUDES}")
    target_include_directories(${target} PUBLIC ${ARGS_INCLUDES})
  endif()
  if(ARGS_DEFINITIONS)
    message(DEBUG "${target}: DEFINITIONS = ${ARGS_DEFINITIONS}")
    target_compile_definitions(${target} PUBLIC ${ARGS_DEFINITIONS})
  endif()
endfunction()

function(add_external_library target library_type)
  set(oneValueArgs GENERATOR PREPEND_PATH LANGUAGE LINKER_LANGUAGE
      LISTS_DIR BUILD_DIR SOURCE_DIR PARENT_TARGET COPY_SOURCES)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES DEFINITIONS PROPERTIES
      COMPILE_FLAGS CONFIG_ARGUMENTS BUILD_ARGUMENTS PRESERVE_VARIABLES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  parse_properties()
  include(GeneralTools)
  if(NOT ARGS_LISTS_DIR)
    set(ARGS_LISTS_DIR "${CMAKE_CURRENT_BINARY_DIR}/${target}")
    # In root build to keep paths sorter on windows
    # set(ARGS_LISTS_DIR "${CMAKE_BINARY_DIR}/${target}")
  endif()
  if(NOT ARGS_SOURCE_DIR)
    set(ARGS_SOURCE_DIR "${ARGS_LISTS_DIR}/src")
  endif()
  if(NOT ARGS_BUILD_DIR)
    set(ARGS_BUILD_DIR "${ARGS_LISTS_DIR}")
  endif()
  if(ARGS_LANGUAGE STREQUAL "Fortran")
    set_default_property(Fortran_MODULE_DIRECTORY "${ARGS_BUILD_DIR}")
  endif()
  if(NOT ARGS_LANGUAGE)
    if(NOT ARGS_SOURCES)
      message(FATAL_ERROR "LANGUAGE not set and SOURCES not provided")
    endif()
    list(GET SOURCES 0 FIRST_SOURCE)
    file2language(${FIRST_SOURCE} ARGS_LANGUAGE)
  endif()
  set(final_library_type ${library_type})
  if(library_type STREQUAL "OBJECT")
    set(final_library_type STATIC)
    # set(ARGS_LINKER_LANGUAGE)  # Prevent enabling unrequired language
  endif()

  # Get source & object file names
  set(EXTERNAL_SOURCES)
  foreach(src IN LISTS ARGS_SOURCES)
    if(NOT IS_ABSOLUTE "${src}")
      cmake_path(
        ABSOLUTE_PATH src
        BASE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" NORMALIZE
        OUTPUT_VARIABLE src
      )
    endif()
    if(ARGS_COPY_SOURCES)
      cmake_path(
        RELATIVE_PATH src
        BASE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        OUTPUT_VARIABLE srcbase
      )
      cmake_path(
        ABSOLUTE_PATH srcbase
        BASE_DIRECTORY "${ARGS_SOURCE_DIR}"
        OUTPUT_VARIABLE srclocal
      )
      configure_file("${src}" "${srclocal}" COPYONLY)
      set(src "${srclocal}")
    endif()
    # list(APPEND EXTERNAL_SOURCES "${src}")
    cmake_path(
      RELATIVE_PATH src
      BASE_DIRECTORY "${ARGS_SOURCE_DIR}"
      OUTPUT_VARIABLE srcbase
    )
    list(APPEND EXTERNAL_SOURCES "${srcbase}")
  endforeach()
  message(DEBUG "EXTERNAL_SOURCES = ${EXTERNAL_SOURCES}")
  set(EXTERNAL_OBJECTS)
  foreach(src IN LISTS ARGS_SOURCES)
    cmake_path(GET src FILENAME src_base)
    cmake_path(
      APPEND ARGS_BUILD_DIR
      "${src_base}${CMAKE_${ARGS_LANGUAGE}_OUTPUT_EXTENSION}"
      OUTPUT_VARIABLE obj
    )
    list(APPEND EXTERNAL_OBJECTS ${obj})
  endforeach()
  message(DEBUG "EXTERNAL_OBJECTS = ${EXTERNAL_OBJECTS}")
  set(EXTERNAL_PRODUCTS ${EXTERNAL_OBJECTS})
  
  # file(MAKE_DIRECTORY "${ARGS_LISTS_DIR}")
  # file(MAKE_DIRECTORY "${ARGS_BUILD_DIR}")
  # file(MAKE_DIRECTORY "${ARGS_SOURCE_DIR}")
  
  get_supported_languages(
    OTHER_LANGUAGES COMPILED
    EXCLUDE ${ARGS_LANGUAGE}
  )
  show_implicit_libraries(PREFIX "TODO-INTERNAL: ")
  
  # Collect targets & save to file that can be loaded
  include(AddTargetsFromFile)
  select_targets(targets LIBRARIES ${ARGS_LIBRARIES})
  generate_target_file(
    "${target}.targets" TARGETS ${targets}
    DIRECTORY ${ARGS_SOURCE_DIR}
    OUTPUT_VAR internal_target_file
    CUSTOM_TARGET "generate_target_file_${target}"
    VERBOSE
  )

  if(ARGS_GENERATOR)
    find_compiler_external(
      ${ARGS_LANGUAGE} REQUIRED GENERATOR ${ARGS_GENERATOR}
      OUTPUT_COMPILER EXTERNAL_COMPILER
      OUTPUT_LINKER EXTERNAL_LINKER
    )
  else()
    find_compiler_external(
      ${ARGS_LANGUAGE} REQUIRED TRY_GENERATORS ALL
      OUTPUT_GENERATOR ARGS_GENERATOR
      OUTPUT_COMPILER EXTERNAL_COMPILER
      OUTPUT_LINKER EXTERNAL_LINKER
    )
  endif()
  list(APPEND ARGS_CONFIG_ARGUMENTS
       "-DCMAKE_${ARGS_LANGUAGE}_COMPILER:PATH=${EXTERNAL_COMPILER}")
  if(EXTERNAL_LINKER)
    list(APPEND ARGS_CONFIG_ARGUMENTS
         "-DCMAKE_${ARGS_LANGUAGE}_LINKER:PATH=${EXTERNAL_LINKER}")
  endif()
  get_native_directory("${EXTERNAL_COMPILER}" COMPILER_PATH)
  message(DEBUG "COMPILER_PATH = ${COMPILER_PATH}")
  list(APPEND ARGS_PREPEND_PATH "${COMPILER_PATH}")
  set(external_def_file)
  if(ARGS_LANGUAGE STREQUAL "Fortran" AND MSVC)
    # EXTERNAL_COMPILER MATCHES "gfortran")
    if(NOT EXTERNAL_COMPILER MATCHES "gfortran")
      message(FATAL_ERROR "Need gfortran with MSVC ${EXTERNAL_COMPILER}")
    endif()
    list(APPEND ARGS_CONFIG_ARGUMENTS
         -DBUILD_SHARED_LIBS=ON
         -DMSVC_AND_GNU_BUILD=ON
         -DCMAKE_GNUtoMS=ON)
  endif()
  if(WIN32)
    find_program(DLLTOOL_PATH dlltool REQUIRED)
    message(STATUS "DLLTOOL_PATH = ${DLLTOOL_PATH}")
    cmake_path(
      # This version matches the original
      # APPEND CMAKE_CURRENT_BINARY_DIR "${ARGS_PARENT_TARGET}.def"
      # This version allows for multiple defs
      APPEND "${ARGS_BUILD_DIR}" "${target}.def"
      OUTPUT_VARIABLE external_def_file
    )
  endif()
  cmake_path(
    APPEND "${ARGS_SOURCE_DIR}" "${target}.external_targets"
    OUTPUT_VARIABLE external_target_file
  )
  list(APPEND EXTERNAL_PRODUCTS ${external_target_file}
       ${external_def_file})
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/external.CMakeLists.in
    ${ARGS_SOURCE_DIR}/CMakeLists.txt
    @ONLY
  )
  setup_external_config(
    ${ARGS_LISTS_DIR} GENERATOR ${ARGS_GENERATOR}
    SOURCE_DIR ${ARGS_SOURCE_DIR}
    BUILD_DIR ${ARGS_BUILD_DIR}
    ARGUMENTS ${ARGS_CONFIG_ARGUMENTS}
    PRESERVE_VARIABLES ${ARGS_PRESERVE_VARIABLES}
    CLEAR_COMPILERS ${OTHER_LANGUAGES}
  )
  setup_external_build(
    ${ARGS_BUILD_DIR} PREPEND_PATH ${ARGS_PREPEND_PATH}
    ARGUMENTS ${ARGS_BUILD_ARGUMENTS}
  )
  set(external_target_name ${target}_build)
  message(STATUS "GENERATOR = ${ARGS_GENERATOR}")
  message(STATUS "CONFIGURE_COMMAND = ${CONFIGURE_COMMAND}")
  message(STATUS "BUILD_COMMAND = ${BUILD_COMMAND}")
  include(ExternalProject)
  externalproject_add(
    ${external_target_name}
    SOURCE_DIR ${ARGS_SOURCE_DIR}
    BINARY_DIR ${ARGS_BUILD_DIR}
    CONFIGURE_COMMAND ${CONFIGURE_COMMAND}
    BUILD_COMMAND ${BUILD_COMMAND}
    BUILD_ALWAYS 1
    BUILD_BYPRODUCTS ${EXTERNAL_PRODUCTS}
    INSTALL_COMMAND ""
    DEPENDS ${targets} generate_target_file_${target}
  )
  
  # create import library for other projects to link to
  predict_target_filename(
    ${target} ${final_library_type} ${ARGS_LANGUAGE} LIBNAME
    BUILD_DIR ${ARGS_BUILD_DIR}
  )
  if(WIN32 AND ${library_type} STREQUAL "SHARED")
    predict_target_filename(
      ${target} IMPORT ${ARGS_LANGUAGE} IMPNAME
      BUILD_DIR ${ARGS_BUILD_DIR}
    )
  endif()
  SET_SOURCE_FILES_PROPERTIES(
    ${EXTERNAL_PRODUCTS}
    PROPERTIES
    GENERATED true)
  SET_SOURCE_FILES_PROPERTIES(
    ${EXTERNAL_OBJECTS}
    PROPERTIES
    EXTERNAL_OBJECT true
    GENERATED true)
  add_import_library(
    ${target} ${library_type} ${LIBNAME} GLOBAL
    OBJECTS ${EXTERNAL_OBJECTS}
    DEPENDENCIES ${external_target_name}
    LIBRARIES ${ARGS_LIBRARIES}
    DEFINITIONS ${ARGS_DEFINITIONS}
    LINK_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR}
    TARGETS_FILE ${external_target_file}
    DEF_FILE ${external_def_file}
    IMPORT_LIBRARY ${IMPNAME}
  )
  if(ARGS_LANGUAGE STREQUAL "Fortran")
    set_property(
      TARGET ${target} PROPERTY
      Fortran_MODULE_DIRECTORY "${PROPS_Fortran_MODULE_DIRECTORY}"
    )
  endif()
endfunction()

function(add_import_library target library_type library)
  set(options GLOBAL)
  set(oneValueArgs TARGETS_FILE IMPORT_LIBRARY DEF_FILE)
  set(multiValueArgs LIBRARIES DEFINITIONS DEPENDENCIES OBJECTS
      COMPILE_DEFINITIONS COMPILE_OPTIONS
      LINK_DIRECTORIES LINK_LIBRARIES LINK_OPTIONS
      INCLUDE_DIRECTORIES PROPERTIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_GLOBAL)
    set(ARGS_GLOBAL GLOBAL)
  endif()
  add_library(${target} ${library_type} IMPORTED ${ARGS_GLOBAL})
  if(ARGS_DEPENDENCIES)
    add_dependencies(${target} ${ARGS_DEPENDENCIES})
  endif()
  # if(library_type STREQUAL "OBJECT" AND NOT ARGS_OBJECTS)
  #   message(FATAL_ERROR "No OBJECTS provided for OBJECT IMPORT library")
  # endif()
  if(ARGS_DEF_FILE)
    list(APPEND ARGS_LINK_OPTIONS "/DEF:${ARGS_DEF_FILE}")
    list(APPEND ARGS_PROPERTIES MSVC_DEF_FILE "${ARGS_DEF_FILE}")
  endif()
  if(ARGS_LIBRARIES)
    list(APPEND ARGS_LINK_LIBRARIES ${ARGS_LIBRARIES})
  endif()
  if(ARGS_DEFINITIONS)
    list(APPEND ARGS_COMPILE_DEFINITIONS ${ARGS_DEFINITIONS})
  endif()
  set_property(
    TARGET ${target} PROPERTY
    IMPORTED_LOCATION ${library}
  )
  if(ARGS_OBJECTS)
    set_property(
      TARGET ${target} PROPERTY
      PROPERTY IMPORTED_OBJECTS ${ARGS_OBJECTS}
    )
  endif()
  if(ARGS_TARGETS_FILE)
    target_link_from_file(${target} IMPORTED ${ARGS_TARGETS_FILE})
  endif()
  foreach(ivar COMPILE_DEFINITIONS COMPILE_OPTIONS
          LINK_DIRECTORIES LINK_LIBRARIES LINK_OPTIONS
          INCLUDE_DIRECTORIES)
    if(ARGS_${ivar})
      set_property(
        TARGET ${target} PROPERTY
        INTERFACE_${ivar} "${ARGS_${ivar}}"
      )
    endif()
  endforeach()
  if(WIN32 AND ${library_type} STREQUAL "SHARED")
    if(NOT ARGS_IMPORT_LIBRARY)
      message(FATAL_ERROR "IMPORT_LIBRARY must be defined for windows build")
    endif()
    set_property(
      TARGET ${target} PROPERTY
      IMPORTED_IMPLIB ${ARGS_IMPORT_LIBRARY}
    )
  endif()
  if(ARGS_PROPERTIES)
    set_target_properties(${target} PROPERTIES ${ARGS_PROPERTIES})
  endif()
endfunction()

function(copy_files destination)
  set(options REQUIRED)
  set(oneValueArgs REPLACE_EXTENSION OUTPUT REGEX_REPLACE REPLACE REPLACEMENT
      SOURCE_DIRECTORY SOURCE_REGEX)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(ARGS_REQUIRED ON)  # TODO: Temporary
  check_exclusive_options(ARGS REGEX_REPLACE REPLACE)
  if(ARGS_REGEX_REPLACE OR ARGS_REPLACE)
    check_set(ARGS " if REGEX_REPLACE or REPLACE provided" REPLACEMENT)
  endif()
  set(result)
  list(APPEND ARGS_SOURCES ${ARGS_UNPARSED_ARGUMENTS})
  if(ARGS_SOURCE_REGEX)
    if(ARGS_SOURCE_DIRECTORY)
      if(NOT IS_DIRECTORY "${ARGS_SOURCE_DIRECTORY}")
        cmake_path(GET ARGS_SOURCE_DIRECTORY
                   PARENT_PATH ARGS_SOURCE_DIRECTORY)
      endif()
      cmake_path(APPEND ARGS_SOURCE_DIRECTORY "${ARGS_SOURCE_REGEX}"
                 OUTPUT_VARIABLE ARGS_SOURCE_REGEX)
    endif()
    file(GLOB NEW_SOURCES LIST_DIRECTORIES false "${ARGS_SOURCE_REGEX}")
    if(NOT NEW_SOURCES)
      if(ARGS_REQUIRED)
        message(FATAL_ERROR "No sources found matching \"${ARGS_SOURCE_REGEX}\"")
      endif()
    endif()
    list(APPEND ARGS_SOURCES ${NEW_SOURCES})
  endif()
  if(ARGS_REQUIRED AND NOT ARGS_SOURCES)
    message(FATAL_ERROR "No sources provided")
  endif()
  message(DEBUG "Copying files to \"${destination}\": ${ARGS_SOURCES}")
  foreach(isrc ${ARGS_SOURCES})
    file(COPY "${isrc}" DESTINATION "${destination}")
    cmake_path(GET isrc FILENAME isrcbase)
    set(idstbase "${isrcbase}")
    cmake_path(APPEND destination "${idstbase}"
               OUTPUT_VARIABLE idst0)
    if(ARGS_REPLACE_EXTENSION)
      cmake_path(GET isrc EXTENSION iext)
      string(REPLACE "${iext}" "${ARGS_REPLACE_EXTENSION}"
             idstbase "${idstbase}")
    endif()
    if(ARGS_REGEX_REPLACE)
      string(REGEX REPLACE
             "${ARGS_REGEX_REPLACE}" "${ARGS_REPLACEMENT}"
             idstbase "${idstbase}")
    elseif(ARGS_REPLACE)
      string(REPLACE
             "${ARGS_REPLACE}" "${ARGS_REPLACEMENT}"
             idstbase "${idstbase}")
    endif()
    cmake_path(APPEND destination "${idstbase}"
               OUTPUT_VARIABLE idst)
    if(NOT idst STREQUAL "${idst0}")
      file(RENAME "${idst0}" "${idst}")
    endif()
    list(APPEND result "${idst}")
  endforeach()
  message(DEBUG "Copied the following to \"${destination}\": ${ARGS_SOURCES}")
  message(DEBUG "Created files: ${result}")
  if(ARGS_OUTPUT)
    set(${ARGS_OUTPUT} "${result}" PARENT_SCOPE)
  endif()
endfunction()

function(copy_target_files target destination)
  set(oneValueArgs TARGET_TYPE TARGET_LANGUAGE EVENT_TARGET EVENT_TYPE)
  set(multiValueArgs COMPONENTS EXCLUDE_COMPONENTS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_TARGET_TYPE)
    get_target_property(ARGS_TARGET_TYPE ${target} TYPE)
  endif()
  if(NOT ARGS_TARGET_LANGUAGE)
    get_target_property(ARGS_TARGET_LANGUAGE ${target} LANGUAGE)
  endif()
  if(NOT ARGS_EVENT_TARGET)
    set(ARGS_EVENT_TARGET ${target})
  endif()
  if(NOT ARGS_EVENT_TYPE)
    set(ARGS_EVENT_TYPE POST_BUILD)
  endif()
  message(DEBUG "copy_target_files: TARGET = ${target}")
  message(DEBUG "copy_target_files: TARGET_TYPE = ${ARGS_TARGET_TYPE}")
  message(DEBUG "copy_target_files: TARGET_LANGUAGE = ${ARGS_TARGET_LANGUAGE}")
  message(DEBUG "copy_target_files: COMPONENTS = ${ARGS_COMPONENTS}")
  if(NOT ARGS_COMPONENTS)
    if(ARGS_TARGET_TYPE STREQUAL "OBJECT_LIBRARY"
       OR ARGS_TARGET_TYPE STREQUAL "OBJECT")
      list(APPEND ARGS_COMPONENTS OBJECTS)
      if(WIN32)
        list(APPEND ARGS_COMPONENTS DEF)
      endif()
    else()
      list(APPEND ARGS_COMPONENTS LIBRARY)
      if(WIN32)
        list(APPEND ARGS_COMPONENTS DEF)
        if(ARGS_TARGET_TYPE STREQUAL "SHARED_LIBRARY"
           OR ARGS_TARGET_TYPE STREQUAL "SHARED")
          list(APPEND ARGS_COMPONENTS IMPORT_LIBRARY)
        endif()
      endif()
    endif()
    if(ARGS_TARGET_LANGUAGE STREQUAL "Fortran")
      list(APPEND ARGS_COMPONENTS FORTRAN_MOD)
    endif()
  endif()
  foreach(component IN LISTS ARGS_COMPONENTS)
    if(ARGS_EXCLUDE_COMPONENTS)
      list(FIND ARGS_EXCLUDE_COMPONENTS "${component}" IDX_COMPONENT)
      if(NOT IDX_COMPONENT EQUAL -1)
        continue()
      endif()
    endif()
    if(component STREQUAL "OBJECTS")
      set(OBJECT_EVENT_TYPE ${ARGS_EVENT_TYPE})
      if(ARGS_EVENT_TARGET STREQUAL "${target}")
        set(OBJECT_EVENT_TYPE PRE_LINK)
      endif()
      add_custom_command_function(
        copy_files MODULE BuildTools
        TARGET ${ARGS_EVENT_TARGET} ${OBJECT_EVENT_TYPE}
        COMMENT "Copy object files for target \"${target}\""
        FUNCTION_ARGUMENTS ${destination}
        GENERATED_FUNCTION_ARGUMENTS
          SOURCES $<JOIN:$<TARGET_OBJECTS:${target}>,$<SEMICOLON>>
        VERBATIM
      )
    elseif(component STREQUAL "FORTRAN_MOD")
      add_custom_command_function(
        copy_files MODULE BuildTools
        TARGET ${ARGS_EVENT_TARGET} ${ARGS_EVENT_TYPE}
        COMMENT "Copy .mod files for target \"${target}\""
        FUNCTION_ARGUMENTS ${destination} SOURCE_REGEX "*.mod"
        GENERATED_FUNCTION_ARGUMENTS
          SOURCE_DIRECTORY $<TARGET_PROPERTY:${target},Fortran_MODULE_DIRECTORY>
        VERBATIM COMMAND_EXPAND_LISTS
      )
    elseif(component STREQUAL "DEF")
      if(ARGS_TARGET_TYPE STREQUAL "OBJECT_LIBRARY"
         OR ARGS_TARGET_TYPE STREQUAL "OBJECT")
        set(GENERATE_DIRECTORY "$<LIST:GET,$<TARGET_OBJECTS:${target}>,0>")
      else()
        set(GENERATE_DIRECTORY "$<TARGET_FILE_DIR:${target}>")
      endif()
      add_custom_command_function(
        copy_files MODULE BuildTools
        TARGET ${ARGS_EVENT_TARGET} ${ARGS_EVENT_TYPE}
        COMMENT "Copy .def files for target \"${target}\""
        FUNCTION_ARGUMENTS ${destination} SOURCE_REGEX "*.def"
        GENERATED_FUNCTION_ARGUMENTS
          SOURCE_DIRECTORY ${GENERATE_DIRECTORY}
        VERBATIM COMMAND_EXPAND_LISTS
      )
    elseif(component STREQUAL "LIBRARY")
      # add_custom_command_function(
      #   copy_files MODULE BuildTools
      #   TARGET ${ARGS_EVENT_TARGET} ${ARGS_EVENT_TYPE}
      #   COMMENT "Copy library file for target \"${target}\""
      #   FUNCTION_ARGUMENTS ${destination}
      #   GENERATED_FUNCTION_ARGUMENTS
      #     SOURCES $<TARGET_FILE:${target}>
      #     # $<TARGET_IMPORT_FILE:${target}>
      # )
      add_custom_command(
        TARGET ${ARGS_EVENT_TARGET} ${ARGS_EVENT_TYPE}
        COMMAND ${CMAKE_COMMAND} -E echo "External TARGET_FILE ${target}: $<TARGET_FILE:${target}>"
        COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:${target}>" ${destination}
      )
    elseif(component STREQUAL "IMPORT_LIBRARY")
      if (CMAKE_GNUtoMS)
        add_custom_command(
          TARGET ${ARGS_EVENT_TARGET} ${ARGS_EVENT_TYPE}
          COMMAND ${CMAKE_COMMAND} -E echo "External TARGET_LIB_FILE ${target}: $<PATH:REPLACE_EXTENSION,$<TARGET_IMPORT_FILE:${target}>,.lib>"
          COMMAND ${CMAKE_COMMAND} -E copy "$<PATH:REPLACE_EXTENSION,$<TARGET_IMPORT_FILE:${target}>,.lib>" ${destination}
        )
      endif()
      add_custom_command(
        TARGET ${ARGS_EVENT_TARGET} ${ARGS_EVENT_TYPE}
        COMMAND ${CMAKE_COMMAND} -E echo "External TARGET_IMPORT_FILE ${target}: $<TARGET_IMPORT_FILE:${target}>"
        COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_IMPORT_FILE:${target}>" ${destination}
      )
    else()
      message(FATAL_ERROR "Unsupported component \"${component}\"")
    endif()
  endforeach()  
endfunction()
