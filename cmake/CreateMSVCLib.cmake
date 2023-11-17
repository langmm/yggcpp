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
  if(dllafile MATCHES "\\.dll\\.a$")
    # TODO: Find dll
    if(ARGS_DIRECTORIES)
      find_file(dllfile "${CMAKE_SHARED_LIBRARY_PREFIX}${libname}${CMAKE_SHARED_LIBRARY_SUFFIX}" PATHS ${ARGS_DIRECTORIES})
    else()
      find_file(dllfile "${CMAKE_SHARED_LIBRARY_PREFIX}${libname}${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
    if (dllfile STREQUAL "dllfile-NOTFOUND")
      message(STATUS "Could not find ${libname} dll")
      return()
    endif()
    string(REPLACE ".dll.a" ".lib" libfile "${dllafile}")
    string(REPLACE ".dll" ".def" deffile "${dllfile}")
    if(EXISTS "${libfile}")
      message(STATUS "${libfile} already exists")
      return()
    endif()
    if(NOT EXISTS "${deffile}")
      execute_process(COMMAND gendef ${dllfile})
    endif()
    execute_process(COMMAND dlltool -d ${deffile} -D ${dllfile} -l ${libfile})
  endif()
endfunction()

function(create_lib_for_target target)
  if (NOT MSVC)
    return()
  endif()
  set(oneValueArgs LIBFILE DEFFILE EXPFILE SOURCE_TARGET)
  set(multiValueArgs OBJECTS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_LIBFILE)
    set(libfile ${ARGS_LIBFILE})
  else()
    cmake_path(APPEND libfile "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_IMPORT_LIBRARY_PREFIX}${target}.lib")
    set_target_properties(target PROPERTIES IMPORT_SUFFIX ".lib")
  endif()
  if (ARGS_DEFFILE)
    set(deffile ${ARGS_DEFFILE})
  else()
    cmake_path(APPEND deffile "${CMAKE_CURRENT_BINARY_DIR}" "${target}.def")
  endif()
  if (ARGS_EXPFILE)
    set(expfile ${ARGS_EXPFILE})
  else()
    cmake_path(APPEND expfile "${CMAKE_CURRENT_BINARY_DIR}" "${target}_exports${CMAKE_C_OUTPUT_EXTENSION}")
  endif()
  if (ARGS_SOURCE_TARGET)
    set(src_target ${ARGS_SOURCE_TARGET})
  else()
    set(src_target ${target})
  endif()
  message(STATUS "CREATE_LIB(${target}) DEFFILE = ${deffile}")
  message(STATUS "CREATE_LIB(${target}) LIBFILE = ${libfile}")
  message(STATUS "CREATE_LIB(${target}) EXPFILE = ${expfile}")
  
  if (ARGS_OBJECTS)
    add_custom_command(
      TARGET ${target}
      PRE_LINK
      COMMAND ${CMAKE_COMMAND} -E echo "TARGET_OBJECTS for ${src_target} $<TARGET_OBJECTS:${src_target}>"
      COMMAND ${CMAKE_COMMAND} -E echo "Passed OBJECTS for ${src_target} ${ARGS_OBJECTS}"
      COMMAND dlltool --export-all-symbols -z ${deffile} -e ${expfile} ${ARGS_OBJECTS}
      COMMAND_EXPAND_LISTS)
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
  endif()
  add_custom_command(
    TARGET ${target}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E echo "TARGET_IMPORT_FILE for ${target} $<TARGET_IMPORT_FILE:${target}>"
    COMMAND ${CMAKE_COMMAND} -E echo "TARGET_LIB_FILE for ${target} ${libfile}"
    COMMAND LIB /DEF:${deffile} /OUT:${libfile}
    # COMMAND LIB /DEF:${deffile} /OUT:$<TARGET_IMPORT_FILE:${target}>
    # COMMAND dlltool -d ${deffile} -D ${dllfile} -l ${libfile}
    COMMAND_EXPAND_LISTS)
  set_source_files_properties(
    ${deffile}
    PROPERTIES
    HEADER_FILE_ONLY true
    GENERATED true)
  set_source_files_properties(
    ${expfile}
    PROPERTIES
    EXTERNAL_OBJECT true
    GENERATED true)
  if(MSVC)
    target_link_options(${target} PRIVATE /DEF:${deffile})
    target_sources(${target} PRIVATE ${expfile})
  else()
    target_sources(${target} PRIVATE ${deffile} ${expfile})
  endif()
endfunction()