function(generate_target_file target_file)
  set(options NO_CONFIG CREATE_LIB)
  set(oneValueArgs OUTPUT_VAR DIRECTORY CUSTOM_TARGET)
  set(multiValueArgs TARGETS EXTRA_LIBRARIES EXTRA_DIRECTORIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_DIRECTORY)
    cmake_path(APPEND target_file ${ARGS_DIRECTORY} ${target_file})
  endif()
  if(ARGS_TARGETS OR ARGS_EXTRA_LIBRARIES OR ARGS_EXTRA_DIRECTORIES)
    message(STATUS "targets = ${ARGS_TARGETS}")
    message(STATUS "target_file = ${target_file}")
    set(CONTENTS "LIBRARIES;${ARGS_EXTRA_LIBRARIES};DIRECTORIES;${ARGS_EXTRA_DIRECTORIES}")
    if(ARGS_NO_CONFIG)
      if(ARGS_TARGETS)
        file(GENERATE OUTPUT "${target_file}"
             CONTENT "${CONTENTS};LIBRARIES;${ARGS_TARGETS};DIRECTORIES;$<TARGET_FILE_DIR:${ARGS_TARGETS}>")
      else()
        file(GENERATE OUTPUT "${target_file}"
             CONTENT "${CONTENTS}")
      endif()
    else()
      if(ARGS_TARGETS)
        file(GENERATE OUTPUT "${target_file}.$<CONFIG>"
             CONTENT "${CONTENTS};LIBRARIES;${ARGS_TARGETS};DIRECTORIES;$<TARGET_FILE_DIR:${ARGS_TARGETS}>")
      else()
        file(GENERATE OUTPUT "${target_file}.$<CONFIG>"
             CONTENT "${CONTENTS}")
      endif()
    endif()
    if(NOT ARGS_NO_CONFIG)
      add_custom_command(
        COMMAND ${CMAKE_COMMAND} "-E" "copy_if_different" "${target_file}.$<CONFIG>" "${target_file}"
	VERBATIM
	PRE_BUILD
	DEPENDS  "${target_file}.$<CONFIG>"
	OUTPUT   "${target_file}"
	COMMENT  "creating ${target_file} file ({event: PRE_BUILD}, {filename: ${target_file}})")
    endif()
  else()
    set(target_file)
  endif()
  if (ARGS_OUTPUT_VAR)
    set(${ARGS_OUTPUT_VAR} ${target_file} PARENT_SCOPE)
  endif()
  if (ARGS_CUSTOM_TARGET)
    add_custom_target("${ARGS_CUSTOM_TARGET}" DEPENDS ${target_file})
  endif()
  message(STATUS "ARGS_CREATE_LIB = ${ARGS_CREATE_LIB}")
  message(STATUS "ARGS_EXTRA_LIBRARIES = ${ARGS_EXTRA_LIBRARIES}")
  if (ARGS_CREATE_LIB AND ARGS_EXTRA_LIBRARIES)
    include(${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CreateMSVCLib.cmake)
    foreach(lib IN LISTS ARGS_EXTRA_LIBRARIES)
      if (ARGS_EXTRA_DIRECTORIES)
        convert_dlla_to_lib(${lib} DIRECTORIES ${ARGS_EXTRA_DIRECTORIES})
      else()
        convert_dlla_to_lib(${lib})
      endif()
    endforeach()
  endif()
endfunction()

function(target_link_from_file target scope)
  set(multiValueArgs FILES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_FILES)
    set(files ${ARGS_FILES})
  else()
    set(files ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  set(LIBS)
  set(DIRS)
  foreach(src IN LISTS files)
    message(STATUS "Begin file ${src}")
    set(curr)
    set(lines)
    if(EXISTS ${src})
      file(READ ${src} lines)
    endif()
    foreach(x IN LISTS lines)
      message(STATUS "  [${curr}] line ${x}")
      if(x STREQUAL "LIBRARIES")
        set(curr LIBS)
      elseif(x STREQUAL "DIRECTORIES")
        set(curr DIRS)
      elseif(NOT curr)
        message(FATAL_ERROR "Type for current line not set")
      elseif(x)
        list(APPEND ${curr} ${x})
      endif()
    endforeach()
  endforeach()
  if(LIBS)
    message(STATUS "Linking libraries ${LIBS}")
    set(LIBS_FULL)
    foreach(x IN LISTS LIBS)
      find_library(${x}_FOUND NAMES ${x} PATHS ${DIRS})
      message(STATUS "Looking for ${x}: ${x}_FOUND = ${${x}_FOUND}")
      if(NOT ${x}_FOUND STREQUAL "${x}_FOUND-NOTFOUND")
        list(APPEND LIBS_FULL ${${x}_FOUND})
      endif()
    endforeach()
    set(LIBS ${LIBS_FULL})
    message(STATUS "Linking libraries (full) ${LIBS}")
    if(scope STREQUAL "IMPORTED")
      get_target_property(EXISTING_LIBS ${target} INTERFACE_LINK_LIBRARIES)
      if(EXISTING_LIBS)
        list(PREPEND LIBS ${EXISTING_LIBS})
        message(STATUS "Linking libraries (with existing) ${LIBS}")
      endif()
      set_target_properties(${target} PROPERTIES
                            INTERFACE_LINK_LIBRARIES "${LIBS}")
    else()
      target_link_libraries(${target} ${scope} ${LIBS})
    endif()
  endif()
  if(DIRS)
    message(STATUS "Linking directories ${DIRS}")
    if(scope STREQUAL "IMPORTED")
      get_target_property(EXISTING_DIRS ${target} INTERFACE_LINK_DIRECTORIES)
      if(EXISTING_DIRS)
        list(PREPEND DIRS ${EXISTING_DIRS})
        message(STATUS "Linking directories (with existing) ${DIRS}")
      endif()
      set_target_properties(${target} PROPERTIES
                            INTERFACE_LINK_DIRECTORIES "${DIRS}")
    else()
      target_link_directories(${target} ${scope} ${DIRS})
    endif()
  endif()
endfunction()
