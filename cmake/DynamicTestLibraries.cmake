function(add_dynamic_test_library TARGET)
  set(oneValueArgs LANGUAGE)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  message(STATUS "ARGS_SOURCES = ${ARGS_SOURCES}")
  message(STATUS "ARGS_LANGUAGE = ${ARGS_LANGUAGE}")
  if (ARGS_SOURCES)
    set(sources ${ARGS_SOURCES})
  else()
    set(sources ${ARGS_UNPARSED_ARGUMENTS})
  endif()
  if ((ARGS_LANGUAGE STREQUAL "Fortran") AND (FORCE_SPLIT_CXXFORTRAN OR MSVC))
    include(YggAddFortranSubdirectory)
    add_mixed_fortran_library(
      ${TARGET} SHARED LANGUAGE CXX
      SOURCES ${sources}
      LIBRARIES ${ARGS_LIBRARIES}
      INCLUDES ${ARGS_INCLUDES}
    )
  else()
    add_library(${TARGET} SHARED ${sources})
    if (ARGS_LANGUAGE STREQUAL "Fortran")
      set_target_properties(${TARGET} PROPERTIES LINKER_LANGUAGE CXX)
    endif()
    if (ARGS_LIBRARIES)
      target_link_libraries(${TARGET} PUBLIC ${ARGS_LIBRARIES})
    endif()
    if (ARGS_INCLUDES)
      target_include_directories(${TARGET} PUBLIC ${ARGS_INCLUDES})
    endif()
  endif()
  if (WIN32)
    set_target_properties(${TARGET} PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
  endif()
  # TODO: Output directory to file that can be loaded
  include(CheckDLL)
  show_symbols(${TARGET})
endfunction()


function(add_dynamic_dependencies TARGET)
  add_dependencies(${TARGET} ${ARGN})
  cmake_path(GET CMAKE_CURRENT_BINARY_DIR PARENT_PATH PARENT_BINARY_DIR)
  foreach(lib ${ARGN})
    add_custom_command(
      TARGET ${TARGET}
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> ${PARENT_BINARY_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> $<TARGET_FILE_DIR:${TARGET}>
      COMMAND_EXPAND_LISTS
    )
  endforeach()
endfunction()