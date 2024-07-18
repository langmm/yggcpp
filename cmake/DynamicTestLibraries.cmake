function(is_compiled_language LANGUAGE VAR)
  string(TOUPPER ${LANGUAGE} LANGUAGE_UPPER)
  if ((LANGUAGE_UPPER STREQUAL "C") OR
      (LANGUAGE_UPPER STREQUAL "CXX") OR
      (LANGUAGE_UPPER STREQUAL "FORTRAN"))
    set(${VAR} 1 PARENT_SCOPE)
  endif()
endfunction()

function(extension2language EXT VAR)
  if(EXT STREQUAL ".c")
    set(${VAR} C PARENT_SCOPE)
  elseif(EXT STREQUAL ".cpp")
    set(${VAR} CXX PARENT_SCOPE)
  elseif(EXT STREQUAL ".jl")
    set(${VAR} Julia PARENT_SCOPE)
  elseif(EXT STREQUAL ".py")
    set(${VAR} Python PARENT_SCOPE)
  elseif(EXT STREQUAL ".R")
    set(${VAR} R PARENT_SCOPE)
  elseif(EXT STREQUAL ".m")
    set(${VAR} Matlab PARENT_SCOPE)
  else()
    string(REGEX MATCH "[.][fF]((90)|(95)|(03)|(08)|(18))?$" match ${EXT})
    if(match)
      set(${VAR} Fortran PARENT_SCOPE)
    else()
      message(ERROR "Support for extension \"${EXT}\" not implemented")
    endif()
  endif()
endfunction()

function(file2language TARGET VAR)
  cmake_path(GET TARGET EXTENSION TARGET_EXT)
  extension2language(${TARGET_EXT} ${VAR})
  set(${VAR} ${${VAR}} PARENT_SCOPE)
endfunction()

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

function(add_external_test_library TARGET)
  set(oneValueArgs LANGUAGE)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT YGGTEST_DYNAMIC_DIR)
    set(YGGTEST_DYNAMIC_DIR ${CMAKE_BINARY_DIR})
  endif()
  set(YGGTEST_DYNAMIC_DIR ${YGGTEST_DYNAMIC_DIR} PARENT_SCOPE)
  if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET})
    add_subdirectory(${TARGET})
  else()
    if (NOT ARGS_SOURCES)
      set(ARGS_SOURCES ${TARGET})
      cmake_path(REMOVE_EXTENSION TARGET)
    endif()
    if (NOT ARGS_LANGUAGE)
      file2language(${ARGS_SOURCES} ARGS_LANGUAGE)
    endif()
    string(TOUPPER ${ARGS_LANGUAGE} LANGUAGE_UPPER)
    if (NOT (BUILD_${ARGS_LANGUAGE}_LIBRARY OR
             BUILD_${LANGUAGE_UPPER}_LIBRARY))
      return()
    endif()
    is_compiled_language(${ARGS_LANGUAGE} IS_COMPILED)
    if (IS_COMPILED)
      add_dynamic_test_library(
        ${TARGET} GENERATE
	LANGUAGE ${ARGS_LANGUAGE}
	SOURCES ${ARGS_SOURCES}
        ${ARGS_UNPARSED_ARGUMENTS}
      )
    else()
      add_embedded_test_script(
        ${TARGET}
	LANGUAGE ${ARGS_LANGUAGE}
	SOURCES ${ARGS_SOURCES}
	${ARGS_UNPARSED_ARGUMENTS}
      )
    endif()
  endif()
endfunction()

function(add_external_test_libraries)
  set(YGGTEST_DYNAMIC_DIR ${CMAKE_BINARY_DIR})
  list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_DIR="${YGGTEST_DYNAMIC_DIR}")
  set_dynamic_test_property(DYNAMIC_TEST_DEFINITIONS)
  foreach(lib ${ARGN})
    add_external_test_library(${lib})
  endforeach()
  get_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LANGUAGES
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
  set(YGGTEST_DYNAMIC_DIR ${YGGTEST_DYNAMIC_DIR} PARENT_SCOPE)
  set(DYNAMIC_TEST_DEFINITIONS ${DYNAMIC_TEST_DEFINITIONS} PARENT_SCOPE)
  set(DYNAMIC_TEST_LANGUAGES ${DYNAMIC_TEST_LANGUAGES} PARENT_SCOPE)
  set(DYNAMIC_TEST_LIBRARIES ${DYNAMIC_TEST_LIBRARIES} PARENT_SCOPE)
  set(DYNAMIC_TEST_DEPENDENCIES ${DYNAMIC_TEST_DEPENDENCIES} PARENT_SCOPE)
  set(EMBEDDED_TEST_SCRIPTS ${EMBEDDED_TEST_SCRIPTS} PARENT_SCOPE)
endfunction()


function(add_dynamic_test_library TARGET)
  set(options GENERATE)
  set(oneValueArgs LANGUAGE)
  set(multiValueArgs SOURCES LIBRARIES INCLUDES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_SOURCES)
    set(ARGS_SOURCES ${ARGS_SOURCES})
  else()
    set(ARGS_SOURCES ${ARGS_UNPARSED_ARGUMENTS})
    set(ARGS_UNPARSED_ARGUMENTS)
  endif()
  if (NOT ARGS_LANGUAGE)
    file2language(${ARGS_SOURCES} ARGS_LANGUAGE)
  endif()
  if (ARGS_GENERATE)
    if (ARGS_LANGUAGE STREQUAL "C" OR ARGS_LANGUAGE STREQUAL "CXX")
      list(APPEND ARGS_LIBRARIES ${YGG_TARGET_CPP})
    elseif (ARGS_LANGUAGE STREQUAL "Fortran")
      list(APPEND ARGS_LIBRARIES ${YGG_TARGET_FORTRAN})
      list(APPEND ARGS_INCLUDES ${YGG_FORTRAN_MOD_DIR})
    else()
      message(ERROR "Unsupported language \"${ARGS_LANGUAGE}\"")
    endif()
  endif()
  message(STATUS "ARGS_SOURCES = ${ARGS_SOURCES}")
  message(STATUS "ARGS_LANGUAGE = ${ARGS_LANGUAGE}")
  if (NOT YGGTEST_DYNAMIC_DIR)
    set(YGGTEST_DYNAMIC_DIR_SET 1)
    cmake_path(GET CMAKE_BINARY_DIR PARENT_PATH YGGTEST_DYNAMIC_DIR)
  endif()
  if ((ARGS_LANGUAGE STREQUAL "Fortran") AND (FORCE_SPLIT_CXXFORTRAN OR MSVC))
    include(YggAddFortranSubdirectory)
    add_mixed_fortran_library(
      ${TARGET} SHARED LANGUAGE CXX
      SOURCES ${ARGS_SOURCES}
      LIBRARIES ${ARGS_LIBRARIES}
      INCLUDES ${ARGS_INCLUDES}
    )
  else()
    add_library(${TARGET} SHARED ${ARGS_SOURCES})
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
    DYNAMIC_TEST_LANGUAGES
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
  if (YGGTEST_DYNAMIC_DIR_SET)
    list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_DIR="${YGGTEST_DYNAMIC_DIR}")
  endif()
  list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_${TARGET})
  list(APPEND DYNAMIC_TEST_LANGUAGES ${ARGS_LANGUAGE})
  # if (TARGET ${TARGET})
  list(APPEND DYNAMIC_TEST_LIBRARIES ${TARGET})
  # endif()
  list(APPEND DYNAMIC_TEST_DEPENDENCIES ${ARGS_LIBRARIES})
  set_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LANGUAGES
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
endfunction()


function(add_embedded_test_script TARGET)
  set(oneValueArgs LANGUAGE)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_SOURCES)
    set(ARGS_SOURCES ${TARGET})
    cmake_path(REMOVE_EXTENSION TARGET)
  endif()
  if (NOT ARGS_LANGUAGE)
    file2language(${ARGS_SOURCES} ARGS_LANGUAGE)
  endif()
  # TODO: Only disabled until embedded languages added
  # list(FIND YGG_INSTALL_DEPS ${ARGS_LANGUAGE} IDX_LANGUAGE)
  # if (IDX_LANGUAGE STREQUAL "-1")
  #   message(STATUS "Embedded language ${ARGS_LANGUAGE} NOT enabled")
  #   return()
  # endif()
  add_custom_target(${TARGET})
  get_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LANGUAGES
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
  list(APPEND DYNAMIC_TEST_LANGUAGES ${ARGS_LANGUAGE})
  list(APPEND DYNAMIC_TEST_LIBRARIES ${TARGET})
  list(APPEND DYNAMIC_TEST_DEFINITIONS -DYGGTEST_DYNAMIC_${TARGET})
  list(APPEND EMBEDDED_TEST_SCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/${ARGS_SOURCES})
  set_dynamic_test_properties(
    DYNAMIC_TEST_DEFINITIONS
    DYNAMIC_TEST_LANGUAGES
    DYNAMIC_TEST_LIBRARIES
    DYNAMIC_TEST_DEPENDENCIES
    EMBEDDED_TEST_SCRIPTS
  )
endfunction()


function(add_dynamic_dependencies TARGET)
  set(oneValueArgs WORKING_DIR)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  message(STATUS "DYNAMIC_TEST_LANGUAGES[${TARGET}] = ${DYNAMIC_TEST_LANGUAGES}")
  message(STATUS "DYNAMIC_TEST_LIBRARIES[${TARGET}] = ${DYNAMIC_TEST_LIBRARIES}")
  message(STATUS "DYNAMIC_TEST_DEFINITIONS[${TARGET}] = ${DYNAMIC_TEST_DEFINITIONS}")
  message(STATUS "DYNAMIC_TEST_DEPENDENCIES[${TARGET}] = ${DYNAMIC_TEST_DEPENDENCIES}")
  message(STATUS "EMBEDDED_TEST_SCRIPTS[${TARGET}] = ${EMBEDDED_TEST_SCRIPTS}")
  message(STATUS "ARGS_WORKING_DIR = ${ARGS_WORKING_DIR}")
  if (TARGET ${TARGET})
    if (NOT ARGS_WORKING_DIR)
      set(ARGS_WORKING_DIR $<TARGET_FILE_DIR:${TARGET}>)
    endif()
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
        get_target_property(lib_type ${lib} TYPE)
	message(STATUS "HERE: ${lib} ${lib_type}")
	if ((lib_type STREQUAL "EXECUTABLE") OR
	    (lib_type STREQUAL "SHARED_LIBRARY") OR
	    (lib_type STREQUAL "STATIC_LIBRARY"))
          add_custom_command(
            TARGET ${TARGET}
            POST_BUILD
    	    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> ${ARGS_WORKING_DIR}
            COMMAND_EXPAND_LISTS
          )
          copy_required_runtimes(
            ${lib}
            DEPENDENCIES ${DYNAMIC_TEST_DEPENDENCIES}
	    DESTINATION_TARGET ${TARGET}
          )
	endif()
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
  elseif(TEST ${TARGET})
    message(STATUS "TARGET \"${TARGET}\" is a cmake test")
    if (NOT ARGS_WORKING_DIR)
      set(ARGS_WORKING_DIR ${CMAKE_CURRENT_BINARY_DIR})
    endif()
    message(STATUS "ARGS_WORKING_DIR = ${ARGS_WORKING_DIR}")
    set(target_setup ${TARGET}_setup_external)
    foreach(script ${EMBEDDED_TEST_SCRIPTS})
      set(ifixture copy_${script})
      add_test(
        ${ifixture}
	COMMAND ${CMAKE_COMMAND} -E copy ${script} ${ARGS_WORKING_DIR}
      )
      set_tests_properties(
        ${ifixture}
	PROPERTIES FIXTURES_SETUP ${target_setup}
      )
    endforeach()
    set_tests_properties(
      ${TARGET}
      PROPERTIES FIXTURES_REQUIRED ${target_setup}
    )
  else()
    message(STATUS "TARGET \"${TARGET}\" is not a cmake target")
    if (NOT ARGS_WORKING_DIR)
      set(ARGS_WORKING_DIR ${CMAKE_CURRENT_BINARY_DIR})
    endif()
    message(STATUS "ARGS_WORKING_DIR = ${ARGS_WORKING_DIR}")
    foreach(script ${EMBEDDED_TEST_SCRIPTS})
      add_custom_command(
        OUTPUT ${ARGS_WORKING_DIR}/${script}
	COMMAND ${CMAKE_COMMAND} -E copy ${script} ${ARGS_WORKING_DIR}
        COMMAND_EXPAND_LISTS
      )
    endforeach()
  endif()
endfunction()


function(generate_dynamic_tests)
  set(oneValueArgs APPEND_TO)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  foreach(CONFIG_FILE ${ARGS_UNPARSED_ARGUMENTS})
    cmake_path(GET CONFIG_FILE FILENAME OUTPUT_TEMPLATE)
    cmake_path(REMOVE_EXTENSION OUTPUT_TEMPLATE LAST_ONLY)
    foreach(LANGUAGE LIBRARY IN ZIP_LISTS DYNAMIC_TEST_LANGUAGES DYNAMIC_TEST_LIBRARIES)
      message(STATUS "GENERATE_DYNAMIC_TESTS: CONFIG_FILE = ${CONFIG_FILE}, LANGUAGE = ${LANGUAGE}, LIBRARY = ${LIBRARY}")
      string(TOLOWER ${LANGUAGE} language)
      string(REPLACE "language" "${language}" OUTPUT ${OUTPUT_TEMPLATE})
      set(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${OUTPUT})
  
      configure_file(
        ${CONFIG_FILE}
        ${OUTPUT}
        @ONLY)
      if (ARGS_APPEND_TO)
        list(APPEND ${ARGS_APPEND_TO} ${OUTPUT})
      endif()
    endforeach()
  endforeach()
  if (ARGS_APPEND_TO)
    set(${ARGS_APPEND_TO} ${${ARGS_APPEND_TO}} PARENT_SCOPE)
  endif()
endfunction()