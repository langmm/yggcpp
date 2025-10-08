function(install_cmake_modules)
  set(oneValueArgs DESTINATION COMPONENT DIRECTORY
      INCLUDE_PATTERN EXCLUDE_PATTERN)
  set(multiValueArgs INCLUDE EXCLUDE)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_DIRECTORY)
    set(ARGS_DIRECTORY cmake)
  endif()
  if(NOT ARGS_DESTINATION)
    set(ARGS_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake)
  endif()
  if(NOT ARGS_INCLUDE_PATTERN)
    set(ARGS_INCLUDE_PATTERN "*.cmake")
  endif()
  if(NOT ARGS_EXCLUDE_PATTERN)
    set(ARGS_EXCLUDE_PATTERN "Find*")
  endif()
  set(modules ${ARGS_INCLUDE})
  if(EXISTS ${ARGS_DIRECTORY})
    file(GLOB_RECURSE modules_raw LIST_DIRECTORIES false
         "${ARGS_DIRECTORY}/${ARGS_INCLUDE_PATTERN}")
    foreach(f IN LISTS modules_raw)
      if(NOT ((ARGS_EXCLUDE_PATTERN AND (f MATCHES "${ARGS_EXCLUDE_PATTERN}"))
              OR (ARGS_EXCLUDE AND (f STREQUAL "${ARGS_EXCLUDE}"))))
        list(APPEND modules ${f})
      endif()
    endforeach()
  endif()
  if(modules)
    install(
      FILES ${modules}
      DESTINATION ${ARGS_DESTINATION}
    )
  endif()
endfunction()

function(complete_install PROJECT)
  include(GNUInstallDirs)
  set(options DONT_INSTALL_CMAKE_PACKAGING DONT_INSTALL_HEADERS
      DONT_INSTALL_TARGETS DONT_INSTALL_DOCS NESTED_INCLUDEDIR)
  set(oneValueArgs EXPORT VERSION COMPONENT CONFIG_TEMPLATE
      INSTALL_PREFIX INSTALL_LIBDIR INSTALL_BINDIR
      INSTALL_INCLUDEDIR INSTALL_CMAKEDIR INSTALL_DOCDIR
      MODULE_DIR MODULES_INCLUDE_PATTERN MODULES_EXCLUDE_PATTERN
      HEADER_DIR DOC_DIR CONFIG_TEMPLATE_DIR)
  set(multiValueArgs TARGETS HEADERS MODULES_INCLUDE MODULES_EXCLUDE
      DOCS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_EXPORT)
    set(ARGS_EXPORT ${PROJECT}Targets)
  endif()
  if(NOT ARGS_INSTALL_LIBDIR)
    cmake_path(
      APPEND ARGS_INSTALL_PREFIX ${CMAKE_INSTALL_LIBDIR}
      OUTPUT_VARIABLE ARGS_INSTALL_LIBDIR
    )
  endif()
  if(NOT ARGS_INSTALL_BINDIR)
    cmake_path(
      APPEND ARGS_INSTALL_PREFIX ${CMAKE_INSTALL_BINDIR}
      OUTPUT_VARIABLE ARGS_INSTALL_BINDIR
    )
  endif()
  if(NOT ARGS_INSTALL_INCLUDEDIR)
    if(ARGS_NESTED_INCLUDEDIR AND NOT SKBUILD)
      cmake_path(
        APPEND ARGS_INSTALL_PREFIX include ${PROJECT}
        OUTPUT_VARIABLE ARGS_INSTALL_INCLUDEDIR
      )
    else()
      cmake_path(
        APPEND ARGS_INSTALL_PREFIX include
        OUTPUT_VARIABLE ARGS_INSTALL_INCLUDEDIR
      )
    endif()
  endif()
  if(NOT ARGS_INSTALL_CMAKEDIR)
    if(UNIX OR CYGWIN)
      cmake_path(
        APPEND ARGS_INSTALL_LIBDIR cmake ${PROJECT}
        OUTPUT_VARIABLE ARGS_INSTALL_CMAKEDIR
      )
    else()
      cmake_path(
        APPEND ARGS_INSTALL_PREFIX cmake
        OUTPUT_VARIABLE ARGS_INSTALL_CMAKEDIR
      )
    endif()
  endif()
  if(NOT ARGS_INSTALL_DOCDIR)
    cmake_path(
      APPEND ARGS_INSTALL_PREFIX share doc ${PROJECT}
      OUTPUT_VARIABLE ARGS_INSTALL_DOCDIR
    )
  endif()
  set(COMPONENT_ARGS)
  if(ARGS_COMPONENT)
    list(APPEND COMPONENT ${ARGS_COMPONENT})
  endif()
  if(ARGS_TARGETS AND NOT ARGS_DONT_INSTALL_TARGETS)
    install(
      TARGETS ${ARGS_TARGETS}
      EXPORT ${ARGS_EXPORT}
      LIBRARY DESTINATION ${ARGS_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${ARGS_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${ARGS_INSTALL_BINDIR}
      PUBLIC_HEADER DESTINATION ${ARGS_INSTALL_INCLUDEDIR}
      ${COMPONENT_ARGS}
    )
    if(NOT ARGS_DONT_INSTALL_CMAKE_PACKAGING)
      install(
        EXPORT ${ARGS_EXPORT}
        FILE ${ARGS_EXPORT}.cmake
        NAMESPACE ${PROJECT}::
        DESTINATION ${ARGS_INSTALL_CMAKEDIR}
        ${COMPONENT_ARGS}
      )
    endif()
  endif()
  if(NOT ARGS_DONT_INSTALL_HEADERS)
    if(ARGS_HEADERS)
      install(
        FILES ${ARGS_HEADERS}
        DESTINATION ${ARGS_INSTALL_INCLUDEDIR}
        ${COMPONENT_ARGS}
      )
    endif()
    if(ARGS_HEADER_DIR)
      install(
        DIRECTORY ${ARGS_HEADER_DIR}
        DESTINATION ${ARGS_INSTALL_INCLUDEDIR}
        ${COMPONENT_ARGS}
      )
    endif()
  endif()

  ###################################
  # CMake modules & Packaging
  ###################################

  if(NOT ARGS_DONT_INSTALL_CMAKE_PACKAGING)
    include(CMakePackageConfigHelpers)
    if(ARGS_VERSION)
      write_basic_package_version_file(
        "${PROJECT}ConfigVersion.cmake"
        VERSION ${ARGS_VERSION}
        COMPATIBILITY AnyNewerVersion
      )
      list(
        APPEND ARGS_MODULES_INCLUDE
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT}ConfigVersion.cmake"
      )
    endif()
    if(NOT ARGS_MODULE_DIR)
      cmake_path(
        APPEND CMAKE_CURRENT_LIST_DIR cmake
        OUTPUT_VARIABLE ARGS_MODULE_DIR
      )
      if(NOT EXISTS ${ARGS_MODULE_DIR})
        set(ARGS_MODULE_DIR)
      endif()
    endif()
    if(NOT ARGS_CONFIG_TEMPLATE)
      set(ARGS_CONFIG_TEMPLATE "${PROJECT}Config.cmake.in")
      if((NOT EXISTS ${ARGS_CONFIG_TEMPLATE}) AND ARGS_MODULE_DIR)
        cmake_path(
          APPEND ARGS_MODULE_DIR ${ARGS_CONFIG_TEMPLATE}
          OUTPUT_VARIABLE ARGS_CONFIG_TEMPLATE
        )
      endif()
      if(NOT EXISTS ${ARGS_CONFIG_TEMPLATE})
        set(ARGS_CONFIG_TEMPLATE)
      endif()
    endif()
    if(ARGS_CONFIG_TEMPLATE)
      configure_file(
        ${ARGS_CONFIG_TEMPLATE}
        ${PROJECT}Config.cmake @ONLY
      )
      list(
        APPEND ARGS_MODULES_INCLUDE
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT}Config.cmake"
      )
    endif()
    if(ARGS_MODULES_INCLUDE OR ARGS_MODULE_DIR)
      install_cmake_modules(
        DESTINATION ${ARGS_INSTALL_CMAKEDIR}
        DIRECTORY ${ARGS_MODULE_DIR}
        INCLUDE ${ARGS_MODULES_INCLUDE}
        EXCLUDE ${ARGS_MODULES_EXCLUDE}
        INCLUDE_PATTERN ${ARGS_MODULES_INCLUDE_PATTERN}
        EXCLUDE_PATTERN ${ARGS_MODULES_EXCLUDE_PATTERN}
        ${COMPONENT_ARGS}
      )
    endif()
    if(ARGS_CONFIG_TEMPLATE_DIR)
      install(
        DIRECTORY ${ARGS_CONFIG_TEMPLATE_DIR}
        DESTINATION ${ARGS_INSTALL_CMAKEDIR}
        FILES_MATCHING REGEX ".*\\.in$"
      )
    endif()
  endif()
  
  ###################################
  # Documentation
  ###################################
  if(NOT ARGS_DONT_INSTALL_DOCS)
    if(ARGS_DOCS)
      install(
        FILES ${ARGS_DOCS}
        DESTINATION ${ARGS_INSTALL_DOCDIR}
        OPTIONAL
        ${COMPONENT_ARGS}
      )
    endif()
    if(ARGS_DOC_DIR AND EXISTS ARGS_DOC_DIR)
      install(
        DIRECTORY ${ARGS_DOC_DIR}
        DESTINATION ${ARGS_INSTALL_DOCDIR}
        OPTIONAL
        ${COMPONENT_ARGS}
      )
    endif()
  endif()
  
endfunction()
