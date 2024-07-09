function(add_dynamic_test_libraries)
  set(YGGTEST_DYNAMIC_DIR ${CMAKE_BINARY_DIR})
  set(YGGTEST_DYNAMIC_DIR ${CMAKE_BINARY_DIR} PARENT_SCOPE)
  foreach(lib ${ARGN})
    add_subdirectory(${lib})
  endforeach()
  set(DYNAMIC_TEST_LIBRARIES ${ARGN} PARENT_SCOPE)
  set(DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_DIR="${YGGTEST_DYNAMIC_DIR}" PARENT_SCOPE)
endfunction()


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
  if (WIN32 AND NOT ARGS_LANGUAGE STREQUAL "Fortran")
    set_target_properties(${TARGET} PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
  endif()
  include(CheckDLL)
  show_symbols(${TARGET})
  if (YGGTEST_DYNAMIC_DIR)
    add_custom_command(
      TARGET ${TARGET}
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${TARGET}> ${YGGTEST_DYNAMIC_DIR}
      COMMAND_EXPAND_LISTS
    )
  endif()
endfunction()


function(add_dynamic_dependencies TARGET)
  add_dependencies(${TARGET} ${ARGN})
  message(STATUS "YGGTEST_DYNAMIC_DIR = ${YGGTEST_DYNAMIC_DIR}")
  if(YGGTEST_DYNAMIC_DIR)
    target_compile_definitions(${TARGET} PUBLIC ${DYNAMIC_TEST_DEFINITIONS})
  endif()
endfunction()