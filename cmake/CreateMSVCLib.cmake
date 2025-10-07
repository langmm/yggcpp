function(convert_dlla_to_lib libname)
  include(${CMAKE_CURRENT_FUNCTION_LIST_DIR}/SearchTools.cmake)
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
    DEPENDS ${src_target}
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