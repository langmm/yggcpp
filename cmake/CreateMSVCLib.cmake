function(convert_library_mingw2msvc src)
  set(oneValueArgs TARGET)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
endfunction()

macro(_initialize_file_transform name src_ext dst_ext)
  set(oneValueArgs DESTINATION DESTINATION_DIR OUTPUT)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  message(DEBUG "${name}: SOURCE = ${SOURCE}")
  if(NOT EXISTS "${SOURCE}")
    message(FATAL_ERROR "${name}: SOURCE file does not exist: \"${SOURCE}\"")
  endif()
  if(NOT ARGS_DESTINATION_DIR)
    if(ARGS_DESTINATION)
      cmake_path(GET ARGS_DESTINATION PARENT_PATH ARGS_DESTINATION_DIR)
    endif()
    if(NOT ARGS_DESTINATION_DIR)
      set(ARGS_DESTINATION_DIR "${CMAKE_CURRENT_BINARY_DIR}")
    endif()
  endif()
  cmake_path(GET SOURCE FILENAME SOURCE_BASE)
  if(NOT ARGS_DESTINATION)
    string(REPLACE "${src_ext}" "${dst_ext}" RESULT_BASE "${SOURCE_BASE}")
    cmake_path(APPEND ARGS_DESTINATION_DIR "${RESULT_BASE}"
               OUTPUT_VARIABLE ARGS_DESTINATION)
  endif()
  if(EXISTS "${ARGS_DESTINATION}")
    _finalize_file_transform(${name} OFF)
    return()
  endif()
endmacro()

macro(_finalize_file_transform name created)
  if(NOT EXISTS "${ARGS_DESTINATION}")
    message(FATAL_ERROR "${name}: Failed to create \"${ARGS_DESTINATION}\"")
  endif()
  if(created)
    message(STATUS "${name}: Created \"${ARGS_DESTINATION}\"")
  endif()
  if(ARGS_OUTPUT)
    set(${ARGS_OUTPUT} "${ARGS_DESTINATION}" PARENT_SCOPE)
  endif()
endmacro()

function(dll2def SOURCE)
  set(oneValueArgs DESTINATION OUTPUT)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT EXISTS "${SOURCE}")
    message(FATAL_ERROR ".dll file does not exist: \"${SOURCE}\"")
  endif()
  string(REPLACE ".dll" ".def" RESULT "${SOURCE}")
  message(DEBUG "create_msvc_lib_from_def: ${SOURCE}")
  cmake_path(GET SOURCE FILENAME SOURCE_BASE)
  cmake_path(GET SOURCE PARENT_PATH SOURCE_DIR)
  cmake_path(GET RESULT FILENAME RESULT_BASE)
  string(REGEX REPLACE "^lib" "" RESULT_BASE_ALT "${RESULT_BASE}")
  cmake_path(APPEND SOURCE_DIR "${RESULT_BASE_ALT}"
             OUTPUT_VARIABLE RESULT_ALT)
  if(ARGS_DESTINATION AND EXISTS "${ARGS_DESTINATION}")
    message(DEBUG "dll2def: DESTINATION already exists \"${ARGS_DESTINATION}\"")
    set(RESULT "${ARGS_DESTINATION}")
  elseif(EXISTS "${RESULT_ALT}")
    set(RESULT "${RESULT_ALT}")
    message(DEBUG "dll2def: RESULT already exists \"${RESULT}\"")
  elseif(EXISTS "${RESULT}")
    message(DEBUG "dll2def: RESULT already exists \"${RESULT}\"")
  else()
    execute_process(
      COMMAND gendef "${SOURCE_BASE}"
      WORKING_DIRECTORY "${SOURCE_DIR}")
      ${ARGS_UNPARSED_ARGUMENTS}
    )
    if(NOT ((EXISTS "${RESULT}") OR (EXISTS "${RESULT_ALT}")))
      message(FATAL_ERROR "dll2def: Failed to create .def file \"${RESULT}\"")
    endif()
    if((NOT EXISTS "${RESULT}") AND (EXISTS "${RESULT_ALT}"))
      set(RESULT "${RESULT_ALT}")
    endif()
    message(DEBUG "dll2def: Created .def file \"${RESULT}\"")
  endif()
  if(NOT ARGS_DESTINATION)
    set(ARGS_DESTINATION "${RESULT}")
  elseif(NOT ARGS_DESTINATION STREQUAL "${RESULT}")
    message(DEBUG "dll2def: Copying \"${RESULT}\" to \"${ARGS_DESTINATION}\"")
    file(READ "${RESULT}" CONTENTS)
    file(WRITE "${ARGS_DESTINATION}" "${CONTENTS}")
    
  endif()
  if(ARGS_OUTPUT)
    set(${ARGS_OUTPUT} "${ARGS_DESTINATION}" PARENT_SCOPE)
  endif()
endfunction()

function(create_mingw_dlla_from_dll SOURCE)
  _initialize_file_transform(
    create_mingw_dlla_from_dll ".dll" ".dll.a" ${ARGN}
  )
  dll2def("${SOURCE}" OUTPUT SOURCE_DEF)
  create_mingw_dlla_from_def(
    ${SOURCE_DEF} DESTINATION "${ARGS_DESTINATION}"
  )
  _finalize_file_transform(
    create_mingw_dlla_from_dll ON
  )
endfunction()

function(create_mingw_dlla_from_def SOURCE)
  _initialize_file_transform(
    create_mingw_dlla_from_def ".def" ".dll.a" ${ARGN}
  )
  execute_process(
    COMMAND dlltool -U -d "${SOURCE}" -l "${ARGS_DESTINATION}"
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  _finalize_file_transform(
    create_mingw_dlla_from_def ON
  )
endfunction()

function(create_msvc_lib_from_dll SOURCE)
  _initialize_file_transform(
    create_msvc_lib_from_dll ".dll" ".lib" ${ARGN}
  )
  dll2def("${SOURCE}" OUTPUT SOURCE_DEF)
  create_msvc_lib_from_def(
    ${SOURCE_DEF} DESTINATION "${ARGS_DESTINATION}"
  )
  _finalize_file_transform(
    create_msvc_lib_from_dll ON
  )
endfunction()

function(create_msvc_lib_from_def SOURCE)
  _initialize_file_transform(
    create_msvc_lib_from_def ".def" ".lib" ${ARGN}
  )
  execute_process(
    COMMAND LIB "/DEF:${SOURCE}" "/OUT:${ARGS_DESTINATION}"
    ${ARGS_UNPARSED_ARGUMENTS}
  )
  _finalize_file_transform(
    create_msvc_lib_from_def ON
  )
endfunction()

function(convert_dlla_to_lib libname)
  include(SearchTools)
  if (NOT WIN32)
    return()
  endif()
  set(oneValueArgs OUTPUT)
  set(multiValueArgs DIRECTORIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(dllavarname ${libname}_dlla)
  if(ARGS_DIRECTORIES)
    find_library(${dllavarname} ${libname} PATHS ${ARGS_DIRECTORIES})
  else()
    find_library(${dllavarname} ${libname})
  endif()
  set(dllafile "${${dllavarname}}")
  if(NOT dllafile MATCHES ".*\\.dll\\.a$")
    if (ARGS_OUTPUT)
      set(${ARGS_OUTPUT} ${dllafile} PARENT_SCOPE)
    endif()
    return()
  endif()
  set(dllvarname ${libname}_dll)
  set(${dllvarname})
  find_library_suffix(${dllvarname} ${libname} SHARED
                      PATHS ${ARGS_DIRECTORIES})
  set(dllfile "${${dllvarname}}")
  if (dllfile STREQUAL "${dllvarname}-NOTFOUND")
    message(STATUS "DDLA2LIB: Could not find ${libname} dll")
    if (ARGS_OUTPUT)
      set(${ARGS_OUTPUT} ${dllafile} PARENT_SCOPE)
    endif()
    return()
  endif()
  string(REPLACE ".dll.a" ".lib" libfile "${dllafile}")
  string(REPLACE ".dll" ".def" deffile "${dllfile}")
  string(REPLACE ".dll" ".exp" expfile "${dllfile}")
  cmake_path(GET libfile FILENAME libbase)
  cmake_path(GET libfile PARENT_PATH libdir)
  string(REGEX REPLACE "^lib" "" libbase_alt ${libbase})
  cmake_path(APPEND libdir ${libbase_alt} OUTPUT_VARIABLE libfile_alt)
  if(NOT EXISTS "${libfile}")
    set(libfile ${libfile_alt})
  endif()
  if (ARGS_OUTPUT)
    set(${ARGS_OUTPUT} ${libfile} PARENT_SCOPE)
  endif()
  if(EXISTS "${libfile}")
    message(STATUS "DLLA2LIB: ${libfile} already exists")
    return()
  endif()
  if(EXISTS "${deffile}")
    message(STATUS "DLLA2LIB: ${deffile} already exists")
  else()
    cmake_path(GET dllfile FILENAME dllbase)
    cmake_path(GET dllfile PARENT_PATH dlldir)
    cmake_path(GET deffile FILENAME defbase)
    string(REGEX REPLACE "^lib" "" defbase_alt ${defbase})
    cmake_path(APPEND dlldir ${defbase_alt} OUTPUT_VARIABLE deffile_alt)
    execute_process(COMMAND gendef ${dllbase}
                    WORKING_DIRECTORY ${dlldir}
	            COMMAND_ERROR_IS_FATAL ANY)
    if(NOT EXISTS "${deffile}")
      if(EXISTS "${deffile_alt}")
        message(STATUS "DLLA2LIB: Stripped def file exists: ${deffile_alt}")
	set(deffile ${deffile_alt})
      else()
        message(FATAL_ERROR "Failed to create .def file ${deffile}")
      endif()
    endif()
  endif()
  execute_process(COMMAND dlltool -d ${deffile} -D ${dllfile} -l ${libfile} -e ${expfile} COMMAND_ERROR_IS_FATAL ANY)
  if(NOT EXISTS "${libfile}")
    message(FATAL_ERROR "Failed to create .lib file ${libfile}")
  else()
    message(STATUS "DLLA2LIB: Created ${libfile}")
  endif()
endfunction()

function(create_lib_for_target target)
  if (NOT MSVC)
    return()
  endif()
  set(oneValueArgs SOURCE_TARGET)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_SOURCE_TARGET)
    set(src_target ${ARGS_SOURCE_TARGET})
  else()
    set(src_target ${target})
  endif()
  # set(expfile "${target}.exp")
  set(deffile "${target}.def")
  
  add_custom_command(
    TARGET ${target}
    # DEPENDS ${src_target}
    PRE_LINK
    COMMAND ${CMAKE_COMMAND} -E echo "TARGET_OBJECTS for ${src_target} $<TARGET_OBJECTS:${src_target}>"
    COMMAND dlltool --export-all-symbols -z $<TARGET_FILE_DIR:${target}>\\${deffile} $<TARGET_OBJECTS:${src_target}>
    BYPRODUCTS ${deffile}
    COMMAND_EXPAND_LISTS)
  # add_custom_command(
  #   TARGET ${target}
  #   POST_BUILD
  #   COMMAND ${CMAKE_COMMAND} -E echo "TARGET_IMPORT_FILE for ${target} $<TARGET_IMPORT_FILE:${target}>"
  #   COMMAND LIB /DEF:$<TARGET_FILE_DIR:${target}>\\${deffile} /NAME:$<PATH:GET_FILENAME,$<TARGET_FILE:${target}>> /OUT:$<PATH:REPLACE_EXTENSION,$<TARGET_IMPORT_FILE:${target}>,.lib>
  #   # COMMAND dlltool -d ${deffile} -D ${dllfile} -l ${libfile}
  #   COMMAND_EXPAND_LISTS)
  # set_source_files_properties(
  #   ${deffile}
  #   PROPERTIES
  #   HEADER_FILE_ONLY true
  #   GENERATED true)
  # set_source_files_properties(
  #   ${expfile}
  #   PROPERTIES
  #   EXTERNAL_OBJECT true
  #   GENERATED true)
  if(MSVC)
    target_link_options(${target} PRIVATE /DEF:$<TARGET_FILE_DIR:${target}>\\${deffile})
    # target_sources(${target} PRIVATE ${expfile})
  else()
    # target_sources(${target} PRIVATE ${deffile} ${expfile})
  endif()
endfunction()