function(get_dynamic_test_properties)
  if (YGGTEST_DYNAMIC_DIR)
    foreach(NAME ${ARGN})
      get_property(
        ${NAME} DIRECTORY ${YGGTEST_DYNAMIC_DIR}
        PROPERTY ${NAME}
      )
      set(${NAME} ${${NAME}} PARENT_SCOPE)
    endforeach()
  endif()
endfunction()

function(set_dynamic_test_properties)
  if (YGGTEST_DYNAMIC_DIR)
    foreach(NAME ${ARGN})
      set_property(
        DIRECTORY ${YGGTEST_DYNAMIC_DIR}
        PROPERTY ${NAME} ${${NAME}}
      )
    endforeach()
  endif()
endfunction()

function(get_dynamic_test_property NAME)
  get_dynamic_test_properties(${NAME})
  set(${NAME} ${${NAME}} PARENT_SCOPE)
endfunction()

function(set_dynamic_test_property NAME)
  set_dynamic_test_properties(${NAME})
endfunction()

function(add_dynamic_test_libraries)
  set(YGGTEST_DYNAMIC_DIR ${CMAKE_BINARY_DIR})
  list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_DIR="${YGGTEST_DYNAMIC_DIR}")
  set_dynamic_test_property(DYNAMIC_TEST_DEFINITIONS)
  foreach(lib ${ARGN})
    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${lib})
      add_subdirectory(${lib})
    else()
      add_embedded_test_script(${lib})
    endif()
  endforeach()
  get_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
  # if (EMBEDDED_TEST_SCRIPTS)
  #   add_custom_target(embedded_scripts DEPENDS ${EMBEDDED_TEST_SCRIPTS})
  # endif()
  set(YGGTEST_DYNAMIC_DIR ${YGGTEST_DYNAMIC_DIR} PARENT_SCOPE)
  set(DYNAMIC_TEST_DEFINITIONS ${DYNAMIC_TEST_DEFINITIONS} PARENT_SCOPE)
  set(DYNAMIC_TEST_LIBRARIES ${DYNAMIC_TEST_LIBRARIES} PARENT_SCOPE)
  set(DYNAMIC_TEST_DEPENDENCIES ${DYNAMIC_TEST_DEPENDENCIES} PARENT_SCOPE)
  set(EMBEDDED_TEST_SCRIPTS ${EMBEDDED_TEST_SCRIPTS} PARENT_SCOPE)
endfunction()


function(add_dynamic_test_library TARGET)
  set(oneValueArgs LANGUAGE)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  message(STATUS "ARGS_SOURCES = ${ARGS_SOURCES}")
  message(STATUS "ARGS_LANGUAGE = ${ARGS_LANGUAGE}")
  if (NOT YGGTEST_DYNAMIC_DIR)
    set(YGGTEST_DYNAMIC_DIR_SET 1)
    cmake_path(GET CMAKE_BINARY_DIR PARENT_PATH YGGTEST_DYNAMIC_DIR)
  endif()
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
  add_custom_command(
    TARGET ${TARGET}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${TARGET}> ${YGGTEST_DYNAMIC_DIR}
    COMMAND_EXPAND_LISTS
  )
  copy_required_runtimes(
    ${TARGET}
    DEPENDENCIES ${ARGS_LIBRARIES}
    DESTINATION ${YGGTEST_DYNAMIC_DIR}
  )
  get_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
  if (YGGTEST_DYNAMIC_DIR_SET)
    list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_DIR="${YGGTEST_DYNAMIC_DIR}")
  endif()
  list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_${TARGET})
  if (TARGET ${TARGET})
    list(APPEND DYNAMIC_TEST_LIBRARIES ${TARGET})
  endif()
  list(APPEND DYNAMIC_TEST_DEPENDENCIES ${ARGS_LIBRARIES})
  set(DYNAMIC_TEST_DEFINITIONS ${DYNAMIC_TEST_DEFINITIONS} PARENT_SCOPE)
  set(DYNAMIC_TEST_LIBRARIES ${DYNAMIC_TEST_LIBRARIES} PARENT_SCOPE)
  set(DYNAMIC_TEST_DEPENDENCIES ${DYNAMIC_TEST_DEPENDENCIES} PARENT_SCOPE)
  set(EMBEDDED_TEST_SCRIPTS ${EMBEDDED_TEST_SCRIPTS} PARENT_SCOPE)
  set_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
endfunction()


function(add_embedded_test_script TARGET)
  if (NOT YGGTEST_DYNAMIC_DIR)
    set(YGGTEST_DYNAMIC_DIR ${CMAKE_BINARY_DIR})
  endif()
  # if (YGGTEST_DYNAMIC_DIR)
  #   add_custom_command(
  #     OUTPUT ${TARGET}
  #     COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET} ${TARGET}
  #     DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET}
  #     VERBATIM
  #   )
  # endif()
  cmake_path(REMOVE_EXTENSION TARGET OUTPUT_VARIABLE TARGET_NOEXT)
  get_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
  list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_${TARGET_NOEXT})
  list(APPEND EMBEDDED_TEST_SCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET})
  set_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
endfunction()


function(add_dynamic_dependencies TARGET)
  set(oneValueArgs WORKING_DIR)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  message(STATUS "DYNAMIC_TEST_LIBRARIES[${TARGET}] = ${DYNAMIC_TEST_LIBRARIES}")
  message(STATUS "DYNAMIC_TEST_DEFINITIONS[${TARGET}] = ${DYNAMIC_TEST_DEFINITIONS}")
  message(STATUS "DYNAMIC_TEST_DEPENDENCIES[${TARGET}] = ${DYNAMIC_TEST_DEPENDENCIES}")
  message(STATUS "EMBEDDED_TEST_SCRIPTS[${TARGET}] = ${EMBEDDED_TEST_SCRIPTS}")
  message(STATUS "ARGS_WORKING_DIR = ${ARGS_WORKING_DIR}")
  if (NOT ARGS_WORKING_DIR)
    set(ARGS_WORKING_DIR $<TARGET_FILE_DIR:${TARGET}>)
  endif()
  if (TARGET ${TARGET})
    if (DYNAMIC_TEST_LIBRARIES)
      add_dependencies(${TARGET} ${DYNAMIC_TEST_LIBRARIES})
    endif()
    if (TARGET embedded_scripts)
      add_dependencies(${TARGET} embedded_scripts)
    endif()
    if(DYNAMIC_TEST_DEFINITIONS)
      target_compile_definitions(${TARGET} PUBLIC ${DYNAMIC_TEST_DEFINITIONS})
    endif()
    if (DYNAMIC_TEST_LIBRARIES AND DYNAMIC_TEST_DEPENDENCIES)
      foreach(lib ${DYNAMIC_TEST_LIBRARIES})
        copy_required_runtimes(
          ${lib}
          DEPENDENCIES ${DYNAMIC_TEST_DEPENDENCIES}
	  DESTINATION_TARGET ${TARGET}
        )
      endforeach()
    endif()
    foreach(script ${EMBEDDED_TEST_SCRIPTS})
      add_custom_command(
        TARGET ${TARGET}
        POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy ${script} ${ARGS_WORKING_DIR}
        COMMAND_EXPAND_LISTS
      )
    endforeach()
  else()
    message(STATUS "TARGET \"${TARGET}\" is not a cmake target")
  endif()
endfunction()