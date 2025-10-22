include(GeneralTools)
include(SearchTools)

macro(_initialize_file_transform name src_ext dst_ext)
  list(APPEND options VERBOSE)
  list(APPEND oneValueArgs DESTINATION DESTINATION_DIR OUTPUT)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(RESULT_ALT)
  if((NOT SOURCE) AND ARGS_SOURCES)
    list(GET ARGS_SOURCES 0 SOURCE)
  endif()
  if(CMAKE_VERBOSE_MAKEFILE)
    set(ARGS_VERBOSE ON)  # TODO: Remove this once debugging complete
  endif()
  if(ARGS_VERBOSE)
    set(ARGS_VERBOSE VERBOSE)
    set(FLAG_VERBOSE "-v")
  else()
    set(ARGS_VERBOSE)
    set(FLAG_VERBOSE)
  endif()
  if(NOT SOURCE)
    message(FATAL_ERROR "${name}: SOURCE not set")
  endif()
  set(SRC_EXT "${src_ext}")
  set(DST_EXT "${dst_ext}")
  message(DEBUG "${name}: SOURCE = ${SOURCE}")
  if(SRC_EXT)
    if(NOT EXISTS "${SOURCE}")
      message(FATAL_ERROR "${name}: SOURCE file does not exist: \"${SOURCE}\"")
    endif()
  else()
    if(NOT TARGET "${SOURCE}")
      message(FATAL_ERROR "${name}: SOURCE is not a target: \"${SOURCE}\"")
    endif()
  endif()
  if(ARGS_DESTINATION_DIR)
    if(NOT IS_DIRECTORY "${ARGS_DESTINATION_DIR}")
      cmake_path(GET ARGS_DESTINATION_DIR PARENT_PATH ARGS_DESTINATION_DIR)
    endif()
  else()
    if(ARGS_DESTINATION)
      cmake_path(GET ARGS_DESTINATION PARENT_PATH ARGS_DESTINATION_DIR)
    endif()
    if(NOT ARGS_DESTINATION_DIR)
      set(ARGS_DESTINATION_DIR "${CMAKE_CURRENT_BINARY_DIR}")
    endif()
  endif()
  if(SRC_EXT)
    cmake_path(GET SOURCE FILENAME SOURCE_BASE)
  else()
    set(SOURCE_BASE "${SOURCE}")
  endif()
  if(NOT ARGS_DESTINATION)
    set(ARGS_DESTINATION_IS_DEFAULT ON)
    if(SRC_EXT)
      string(REPLACE "${SRC_EXT}" "${DST_EXT}"
             RESULT_BASE "${SOURCE_BASE}")
    else()
      set(RESULT_BASE "${SOURCE_BASE}${DST_EXT}")
    endif()
    cmake_path(APPEND ARGS_DESTINATION_DIR "${RESULT_BASE}"
               OUTPUT_VARIABLE ARGS_DESTINATION)
  endif()
  message(DEBUG "${name}: DESTINATION = ${ARGS_DESTINATION}")
  message(DEBUG "${name}: UNPARSED_ARGUMENTS = ${ARGS_UNPARSED_ARGUMENTS}")
  if(EXISTS "${ARGS_DESTINATION}")
    _finalize_file_transform(${name} OFF)
    return()
  endif()
endmacro()

macro(_finalize_file_transform name created)
  if(NOT EXISTS "${ARGS_DESTINATION}")
    if(RESULT_ALT AND (EXISTS "${RESULT_ALT}"))
      message(DEBUG "${name}: Copying \"${RESULT_ALT}\" to \"${ARGS_DESTINATION}\"")
      # TODO: use configure_file to create cmake dependency?
      # configure_file("${RESULT_ALT}" "${ARGS_DESTINATION}" COPYONLY)
      file(COPY_FILE "${RESULT_ALT}" "${ARGS_DESTINATION}")
    else()
      message(FATAL_ERROR "${name}: Failed to create \"${ARGS_DESTINATION}\"")
    endif()
  endif()
  if(created)
    message(STATUS "${name}: Created \"${ARGS_DESTINATION}\"")
  endif()
  if(ARGS_OUTPUT)
    set(${ARGS_OUTPUT} "${ARGS_DESTINATION}" PARENT_SCOPE)
  endif()
endmacro()

function(obj2def)
  set(options)
  set(oneValueArgs COMMAND_ERROR_IS_FATAL COMMAND_ECHO)
  set(multiValueArgs SOURCES)
  if(WIN32)
    _initialize_file_transform(
      obj2def ".obj" ".def" ${ARGN}
    )
  else()
    _initialize_file_transform(
      obj2def ".o" ".def" ${ARGN}
    )
  endif()
  set_default(ARGS_COMMAND_ERROR_IS_FATAL ANY)
  set_default(ARGS_COMMAND_ECHO STDOUT)
  message(DEBUG "obj2def: SOURCES = ${ARGS_SOURCES}")
  find_program_generic(DLLTOOL dlltool REQUIRED)
  execute_process(
    COMMAND "${DLLTOOL}" ${FLAG_VERBOSE} --export-all-symbols -z "${ARGS_DESTINATION}" ${ARGS_SOURCES}
    COMMAND_ERROR_IS_FATAL ${ARGS_COMMAND_ERROR_IS_FATAL}
    COMMAND_ECHO ${ARGS_COMMAND_ECHO}
    RESULT_VARIABLE RET
    ECHO_OUTPUT_VARIABLE RET_OUTPUT
    ECHO_ERROR_VARIABLE RET_ERROR
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  message(STATUS "ECHO_OUTPUT_VARIABLE = ${ECHO_OUTPUT_VARIABLE}")
  message(STATUS "ECHO_ERROR_VARIABLE = ${ECHO_ERROR_VARIABLE}")
  message(STATUS "RET_OUTPUT = ${RET_OUTPUT}")
  if(RET_OUTPUT)
    message(FATAL_ERROR "RET_OUTPUT = ${RET_OUTPUT}")
  endif()
  _finalize_file_transform(obj2def ON)
endfunction()

function(dll2def SOURCE)
  set(options)
  set(oneValueArgs COMMAND_ERROR_IS_FATAL COMMAND_ECHO)
  set(multiValueArgs)
  _initialize_file_transform(
    dll2def ".dll" ".def" ${ARGN}
  )
  set_default(ARGS_COMMAND_ERROR_IS_FATAL ANY)
  set_default(ARGS_COMMAND_ECHO STDOUT)
  # cmake_path(GET SOURCE FILENAME SOURCE_BASE)
  # cmake_path(GET SOURCE PARENT_PATH SOURCE_DIR)
  cmake_path(GET ARGS_DESTINATION FILENAME RESULT_BASE)
  string(REGEX REPLACE "^lib" "" RESULT_BASE_ALT "${RESULT_BASE}")
  cmake_path(APPEND ARGS_DESTINATION_DIR "${RESULT_BASE_ALT}"
             OUTPUT_VARIABLE RESULT_ALT)
  if(EXISTS "${RESULT_ALT}")
    if(ARGS_DESTINATION_IS_DEFAULT)
      set(ARGS_DESTINATION "${RESULT_ALT}")
      _finalize_file_transform(dll2def OFF)
      return()
    endif()
    message(DEBUG "dll2def: File already exists under a different name \"${RESULT_ALT}\"")
  else()
    find_program_generic(GENDEF gendef REQUIRED)
    execute_process(
      COMMAND "${GENDEF}" "${SOURCE}"
      WORKING_DIRECTORY "${ARGS_DESTINATION_DIR}"
      COMMAND_ERROR_IS_FATAL ${ARGS_COMMAND_ERROR_IS_FATAL}
      COMMAND_ECHO ${ARGS_COMMAND_ECHO}
      ${ARGS_UNPARSED_ARGUMENTS}
    )
  endif()
  _finalize_file_transform(dll2def ON)
endfunction()

function(target2def SOURCE)
  set(options)
  set(oneValueArgs EVENT_TARGET)
  set(multiValueArgs)
  _initialize_file_transform(
    target2def "" ".def" ${ARGN}
  )
  if(ARGS_EVENT_TARGET)
    set(EVENT_TYPE PRE_LINK)
  else()
    set(ARGS_EVENT_TARGET ${SOURCE})
    set(EVENT_TYPE POST_BUILD)
  endif()
  add_custom_command_function(
    obj2def MODULE CreateMSVCLib
    TARGET ${ARGS_EVENT_TARGET} ${EVENT_TYPE}
    COMMAND_ARGUMENTS -DTARGET_OBJECTS=$<JOIN:$<TARGET_OBJECTS:${SOURCE}>,$<SEMICOLON>>
    FUNCTION_ARGUMENTS SOURCES "\$\{TARGET_OBJECTS\}"
      DESTINATION "${ARGS_DESTINATION}" ${ARGS_VERBOSE}
    BYPRODUCTS "${ARGS_DESTINATION}"
    VERBATIM COMMAND_EXPAND_LISTS
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  if(ARGS_OUTPUT)
    set(${ARGS_OUTPUT} "${ARGS_DESTINATION}" PARENT_SCOPE)
  endif()
endfunction()

function(create_mingw_dlla_from_dll SOURCE)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs)
  _initialize_file_transform(
    create_mingw_dlla_from_dll ".dll" ".dll.a" ${ARGN}
  )
  dll2def("${SOURCE}" OUTPUT SOURCE_DEF ${ARGS_VERBOSE})
  create_mingw_dlla_from_def(
    ${SOURCE_DEF} DESTINATION "${ARGS_DESTINATION}" ${ARGS_VERBOSE}
  )
  _finalize_file_transform(
    create_mingw_dlla_from_dll ON
  )
endfunction()

function(create_mingw_dlla_from_def SOURCE)
  set(options)
  set(oneValueArgs COMMAND_ERROR_IS_FATAL COMMAND_ECHO)
  set(multiValueArgs)
  _initialize_file_transform(
    create_mingw_dlla_from_def ".def" ".dll.a" ${ARGN}
  )
  set_default(ARGS_COMMAND_ERROR_IS_FATAL ANY)
  set_default(ARGS_COMMAND_ECHO STDOUT)
  find_program_generic(DLLTOOL dlltool REQUIRED)
  execute_process(
    COMMAND "${DLLTOOL}" ${FLAG_VERBOSE} -U -d "${SOURCE}" -l "${ARGS_DESTINATION}"
    COMMAND_ERROR_IS_FATAL ${ARGS_COMMAND_ERROR_IS_FATAL}
    COMMAND_ECHO ${ARGS_COMMAND_ECHO}
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  _finalize_file_transform(
    create_mingw_dlla_from_def ON
  )
endfunction()

function(create_msvc_lib_from_dll SOURCE)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs)
  _initialize_file_transform(
    create_msvc_lib_from_dll ".dll" ".lib" ${ARGN}
  )
  dll2def("${SOURCE}" OUTPUT SOURCE_DEF ${ARGS_VERBOSE})
  create_msvc_lib_from_def(
    ${SOURCE_DEF} DESTINATION "${ARGS_DESTINATION}" ${ARGS_VERBOSE}
  )
  _finalize_file_transform(
    create_msvc_lib_from_dll ON
  )
endfunction()

function(create_msvc_lib_from_def SOURCE)
  set(options)
  set(oneValueArgs COMMAND_ERROR_IS_FATAL COMMAND_ECHO)
  set(multiValueArgs)
  _initialize_file_transform(
    create_msvc_lib_from_def ".def" ".lib" ${ARGN}
  )
  set_default(ARGS_COMMAND_ERROR_IS_FATAL ANY)
  set_default(ARGS_COMMAND_ECHO STDOUT)
  if(ARGS_VERBOSE)
    set(FLAG_VERBOSE "/VERBOSE")
  endif()
  execute_process(
    COMMAND LIB ${FLAG_VERBOSE} "/DEF:${SOURCE}" "/OUT:${ARGS_DESTINATION}"
    COMMAND_ERROR_IS_FATAL ${ARGS_COMMAND_ERROR_IS_FATAL}
    COMMAND_ECHO ${ARGS_COMMAND_ECHO}
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  _finalize_file_transform(
    create_msvc_lib_from_def ON
  )
endfunction()

function(create_msvc_lib_from_name name)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs DIRECTORIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_DIRECTORIES)
    find_library(SOURCE ${name} PATHS ${ARGS_DIRECTORIES})
  else()
    find_library(SOURCE ${name})
  endif()
  if(SOURCE STREQUAL "SOURCE-NOTFOUND")
    message(FATAL_ERROR "create_msvc_lib_from_name: Failed to find library \"${name}\"")
  endif()
  if(SOURCE MATCHES ".*\\.dll\\.a$")
    set(SOURCE_DLLA "${SOURCE}")
    find_library_suffix(
      SOURCE ${name} SHARED
      PATHS ${ARGS_DIRECTORIES}
    )
    if(SOURCE STREQUAL "SOURCE-NOTFOUND")
      message(FATAL_ERROR "create_msvc_lib_from_name: Failed to find DLL for library \"${name}\"")
    endif()
  endif()
  _initialize_file_transform(
    create_msvc_lib_from_name ".dll" ".lib"
  )
  if(SOURCE MATCHES ".*\\.lib$")
    set(RESULT_ALT ${SOURCE})
  elseif(SOURCE MATCHES ".*\\.dll$")
    collect_arguments(
      NESTED_ARGS ARGS "${options}"
      DESTINATION DESTINATION_DIR OUTPUT VERBOSE
    )
    create_msvc_lib_from_dll(${SOURCE} ${NESTED_ARGS})
  endif()
  _finalize_file_transform(create_msvc_lib_from_name ON)
endfunction()

# function(convert_dlla_to_lib libname)
#   include(SearchTools)
#   if (NOT WIN32)
#     return()
#   endif()
#   set(oneValueArgs OUTPUT)
#   set(multiValueArgs DIRECTORIES)
#   cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
#   set(dllavarname ${libname}_dlla)
#   if(ARGS_DIRECTORIES)
#     find_library(${dllavarname} ${libname} PATHS ${ARGS_DIRECTORIES})
#   else()
#     find_library(${dllavarname} ${libname})
#   endif()
#   set(dllafile "${${dllavarname}}")
#   if(NOT dllafile MATCHES ".*\\.dll\\.a$")
#     if (ARGS_OUTPUT)
#       set(${ARGS_OUTPUT} ${dllafile} PARENT_SCOPE)
#     endif()
#     return()
#   endif()
#   set(dllvarname ${libname}_dll)
#   set(${dllvarname})
#   find_library_suffix(${dllvarname} ${libname} SHARED
#                       PATHS ${ARGS_DIRECTORIES})
#   set(dllfile "${${dllvarname}}")
#   if (dllfile STREQUAL "${dllvarname}-NOTFOUND")
#     message(STATUS "DDLA2LIB: Could not find ${libname} dll")
#     if (ARGS_OUTPUT)
#       set(${ARGS_OUTPUT} ${dllafile} PARENT_SCOPE)
#     endif()
#     return()
#   endif()
#   string(REPLACE ".dll.a" ".lib" libfile "${dllafile}")
#   string(REPLACE ".dll" ".def" deffile "${dllfile}")
#   string(REPLACE ".dll" ".exp" expfile "${dllfile}")
#   cmake_path(GET libfile FILENAME libbase)
#   cmake_path(GET libfile PARENT_PATH libdir)
#   string(REGEX REPLACE "^lib" "" libbase_alt ${libbase})
#   cmake_path(APPEND libdir ${libbase_alt} OUTPUT_VARIABLE libfile_alt)
#   if(NOT EXISTS "${libfile}")
#     set(libfile ${libfile_alt})
#   endif()
#   if (ARGS_OUTPUT)
#     set(${ARGS_OUTPUT} ${libfile} PARENT_SCOPE)
#   endif()
#   if(EXISTS "${libfile}")
#     message(STATUS "DLLA2LIB: ${libfile} already exists")
#     return()
#   endif()
#   if(EXISTS "${deffile}")
#     message(STATUS "DLLA2LIB: ${deffile} already exists")
#   else()
#     cmake_path(GET dllfile FILENAME dllbase)
#     cmake_path(GET dllfile PARENT_PATH dlldir)
#     cmake_path(GET deffile FILENAME defbase)
#     string(REGEX REPLACE "^lib" "" defbase_alt ${defbase})
#     cmake_path(APPEND dlldir ${defbase_alt} OUTPUT_VARIABLE deffile_alt)
#     execute_process(COMMAND gendef ${dllbase}
#                     WORKING_DIRECTORY ${dlldir}
# 	            COMMAND_ERROR_IS_FATAL ANY)
#     if(NOT EXISTS "${deffile}")
#       if(EXISTS "${deffile_alt}")
#         message(STATUS "DLLA2LIB: Stripped def file exists: ${deffile_alt}")
# 	set(deffile ${deffile_alt})
#       else()
#         message(FATAL_ERROR "Failed to create .def file ${deffile}")
#       endif()
#     endif()
#   endif()
#   execute_process(COMMAND dlltool -v -d ${deffile} -D ${dllfile} -l ${libfile} -e ${expfile} COMMAND_ERROR_IS_FATAL ANY)
#   if(NOT EXISTS "${libfile}")
#     message(FATAL_ERROR "Failed to create .lib file ${libfile}")
#   else()
#     message(STATUS "DLLA2LIB: Created ${libfile}")
#   endif()
# endfunction()

# function(create_lib_for_target target)
#   if (NOT MSVC)
#     return()
#   endif()
#   set(oneValueArgs SOURCE_TARGET)
#   cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
#   if (ARGS_SOURCE_TARGET)
#     set(src_target ${ARGS_SOURCE_TARGET})
#   else()
#     set(src_target ${target})
#   endif()
#   # set(expfile "${target}.exp")
#   set(deffile "${target}.def")
  
#   add_custom_command(
#     TARGET ${target}
#     PRE_LINK
#     COMMAND ${CMAKE_COMMAND} -E echo "TARGET_OBJECTS for ${src_target} $<TARGET_OBJECTS:${src_target}>"
#     COMMAND dlltool -v --export-all-symbols -z $<TARGET_FILE_DIR:${target}>\\${deffile} $<TARGET_OBJECTS:${src_target}>
#     BYPRODUCTS ${deffile}
#     COMMAND_EXPAND_LISTS
#   )
#   # add_custom_command(
#   #   TARGET ${target}
#   #   POST_BUILD
#   #   COMMAND ${CMAKE_COMMAND} -E echo "TARGET_IMPORT_FILE for ${target} $<TARGET_IMPORT_FILE:${target}>"
#   #   COMMAND LIB /DEF:$<TARGET_FILE_DIR:${target}>\\${deffile} /NAME:$<PATH:GET_FILENAME,$<TARGET_FILE:${target}>> /OUT:$<PATH:REPLACE_EXTENSION,$<TARGET_IMPORT_FILE:${target}>,.lib>
#   #   # COMMAND dlltool -v -d ${deffile} -D ${dllfile} -l ${libfile}
#   #   COMMAND_EXPAND_LISTS)
#   # set_source_files_properties(
#   #   ${deffile}
#   #   PROPERTIES
#   #   HEADER_FILE_ONLY true
#   #   GENERATED true)
#   # set_source_files_properties(
#   #   ${expfile}
#   #   PROPERTIES
#   #   EXTERNAL_OBJECT true
#   #   GENERATED true)
#   if(MSVC)
#     target_link_options(${target} PRIVATE /DEF:$<TARGET_FILE_DIR:${target}>\\${deffile})
#     # target_sources(${target} PRIVATE ${expfile})
#   else()
#     # target_sources(${target} PRIVATE ${deffile} ${expfile})
#   endif()
# endfunction()