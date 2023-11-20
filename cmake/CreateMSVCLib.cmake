function(convert_dlla_to_lib libname)
  if (NOT MSVC)
    return()
  endif()
  set(multiValueArgs DIRECTORIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_DIRECTORIES)
    find_library(dllafile ${libname} PATHS ${ARGS_DIRECTORIES})
  else()
    find_library(dllafile ${libname})
  endif()
  if(dllafile MATCHES ".*\\.dll\\.a$")
    # TODO: Find dll
    if(ARGS_DIRECTORIES)
      find_file(dllfile "${CMAKE_SHARED_LIBRARY_PREFIX}${libname}${CMAKE_SHARED_LIBRARY_SUFFIX}" PATHS ${ARGS_DIRECTORIES})
    else()
      find_file(dllfile "${CMAKE_SHARED_LIBRARY_PREFIX}${libname}${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
    if (dllfile STREQUAL "dllfile-NOTFOUND")
      message(STATUS "DDLA2LIB: Could not find ${libname} dll")
      return()
    endif()
    string(REPLACE ".dll.a" ".lib" libfile "${dllafile}")
    string(REPLACE ".dll" ".def" deffile "${dllfile}")
    if(EXISTS "${libfile}")
      message(STATUS "DLLA2LIB: ${libfile} already exists")
      return()
    endif()
    if(NOT EXISTS "${deffile}")
      execute_process(COMMAND gendef ${dllfile})
    endif()
    execute_process(COMMAND dlltool -d ${deffile} -D ${dllfile} -l ${libfile})
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