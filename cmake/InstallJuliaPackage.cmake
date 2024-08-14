function(install_julia_package PACKAGE_DIRECTORY)
  set(oneValueArgs Julia_EXECUTABLE DESTINATION TARGET)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_Julia_EXECUTABLE)
    set(Julia_EXECUTABLE ${ARGS_Julia_EXECUTABLE})
  endif()
  if(NOT ARGS_DESTINATION)
    set(ARGS_DESTINATION ${CMAKE_INSTALL_BINDIR})
  endif()
  cmake_path(GET PACKAGE_DIRECTORY FILENAME PACKAGE_NAME)
  cmake_path(
    APPEND ARGS_DESTINATION "${PACKAGE_NAME}"
    OUTPUT_VARIABLE PACKAGE_DESTINATION
  )
  if(NOT ARGS_TARGET)
    set(ARGS_TARGET PACKAGE_NAME)
  endif()
  message(STATUS "PACKAGE_NAME = ${PACKAGE_NAME}")
  message(STATUS "PACKAGE_DIRECTORY = ${PACKAGE_DIRECTORY}")
  message(STATUS "Julia_EXECUTABLE = ${Julia_EXECUTABLE}")
  message(STATUS "PACKAGE_DESTINATION = ${PACKAGE_DESTINATION}")
  message(STATUS "CMAKE_INSTALL_PREFIX = ${CMAKE_INSTALL_PREFIX}")
  if(NOT Julia_EXECUTABLE)
    message(FATAL_ERROR "Julia_EXECUTABLE not set")
  endif()
  set(INSTALL_SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/install_julia_package.jl)
  configure_file(
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/install_julia_package.jl.in
    ${INSTALL_SCRIPT}
    @ONLY
  )
  # TODO: Add ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR} to library
  #   path environment variables? Currently run_tests.sh adds _install
  #   to DYLD_LIBRARY_PATH, but the default _install location should
  #   already be on the path
  # message(STATUS "DYLD_LIBRARY_PATH = $ENV{DYLD_LIBRARY_PATH}")
  # cmake_path(
  #   APPEND CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_LIBDIR}
  #   OUTPUT_VARIABLE LIBRARY_INSTALL_DIR
  # )
  install(
    CODE
    "execute_process(COMMAND 
                     ${Julia_EXECUTABLE} ${INSTALL_SCRIPT}
                     COMMAND_ERROR_IS_FATAL ANY
		     COMMAND_ECHO STDOUT)"
  )
  # Using BinaryBuilder which only allows Julia 1.7
  # if(TARGET ${ARGS_TARGET})
  #   set(PACKAGE_LIBRARY $<TARGET_FILE:${ARGS_TARGET}>)
  #   cmake_path(GET ${PACKAGE_LIBRARY} FILENAME PACKAGE_LIBRARY)
  #   cmake_path(GET ${PACKAGE_LIBRARY} STEM PACKAGE_LIBRARY)
  # else()
  #   set(PACKAGE_LIBRARY ${CMAKE_SHARED_LIBRARY_PREFIX}${ARGS_TARGET})
  # endif()
  # set(PACKAGE_VERSION ${LIB_VERSION_STRING})
  # TODO: Config specific location on windows?
  # set(BUILD_SCRIPT ${PACKAGE_DIRECTORY}/deps/build.jl)
  # configure_file(
  #   ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/build_BinaryBuilder.jl.in
  #   ${BUILD_SCRIPT}
  #   @ONLY
  # )
  # install(
  #   CODE
  #   "execute_process(COMMAND 
  #                    ${Julia_EXECUTABLE} ${BUILD_SCRIPT} --deploy=local
  #                    COMMAND_ERROR_IS_FATAL ANY
  # 		     COMMAND_ECHO STDOUT)"
  # )
endfunction()