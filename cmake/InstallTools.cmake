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

function(get_library_parts LIBPATH OUTPUT_ROOT)
  cmake_path(GET LIBPATH STEM LAST_ONLY libname)
  cmake_path(GET LIBPATH PARENT_PATH libdir)
  if(libname MATCHES "^lib(.+)")
    set(libname "${CMAKE_MATCH_1}")
    # string(SUBSTRING "${libname}" 3 -1 libname)
  endif()
  set(${OUTPUT_ROOT}_LIBNAME "${libname}" PARENT_SCOPE)
  set(${OUTPUT_ROOT}_LIBDIR "${libdir}" PARENT_SCOPE)
endfunction()

function(get_generator_expresion_args INPUT OUTPUT_VAR)
  # TODO: Handle remainder
  set(oneValueArgs VALUE_VAR)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_VALUE_VAR)
    if(INPUT MATCHES "^.?<(.+)\:(.*)>(\:.+)?$")
      set(${OUTPUT_VAR} "${CMAKE_MATCH_1}" PARENT_SCOPE)
      set(${ARGS_VALUE_VAR} "${CMAKE_MATCH_2}" PARENT_SCOPE)
      return()
    endif()
  endif()
  if(INPUT MATCHES "^.?<(.+)>(\:.+)?$")
    set(${OUTPUT_VAR} "${CMAKE_MATCH_1}" PARENT_SCOPE)
    if(ARGS_VALUE_VAR)
      set(${ARGS_VALUE_VAR} "" PARENT_SCOPE)
    endif()
    return()
  endif()
  set(${OUTPUT_VAR} "" PARENT_SCOPE)
  if(ARGS_VALUE_VAR)
    set(${ARGS_VALUE_VAR} "" PARENT_SCOPE)
  endif()
endfunction()

function(get_target_install_property OUTPUT_VAR TARGET PROPERTY)
  set(options APPEND RECURSIVE COMPLETE)
  set(oneValueArgs COMPILE_LANGUAGE LINK_LANGUAGE)
  set(multiValueArgs LOCAL_TARGETS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_LINK_LANGUAGE)
    get_target_property(ARGS_LINK_LANGUAGE ${TARGET} LINKER_LANGUAGE)
  endif()
  if(NOT ARGS_COMPILE_LANGUAGE)
    set(ARGS_COMPILE_LANGUAGE ${ARGS_LINK_LANGUAGE})
  endif()
  set(PROPERTY_DIR)
  if(PROPERTY STREQUAL "INCLUDE_DIRECTORIES"
     OR PROPERTY STREQUAL "LINK_DIRECTORIES")
    set(PROPERTY_DIR ${CMAKE_INSTALL_PREFIX})
  endif()
  if(NOT ARGS_APPEND)
    set(${OUTPUT_VAR})
  endif()
  set(RECURSIVE_ARGS
      COMPILE_LANGUAGE ${ARGS_COMPILE_LANGUAGE}
      LINK_LANGUAGE ${ARGS_LINK_LANGUAGE})
  if(ARGS_COMPLETE)
    list(APPEND RECURSIVE_ARGS COMPLETE)
  endif()
  if(ARGS_LOCAL_TARGETS)
    list(APPEND RECURSIVE_ARGS LOCAL_TARGETS ${ARGS_LOCAL_TARGETS})
  endif()
  set(property_name "INTERFACE_${PROPERTY}")
  get_target_property(RAW_OUTPUT ${TARGET} ${property_name})
  if(RAW_OUTPUT STREQUAL "RAW_OUTPUT-NOTFOUND")
    set(RAW_OUTPUT)
  # else()
  #   message(DEBUG "${property_name} ${TARGET}: ${RAW_OUTPUT}")
  endif()
  foreach(vraw IN LISTS RAW_OUTPUT)
    set(v "${vraw}")
    get_generator_expresion_args("${vraw}" key VALUE_VAR val)
    if(key)
      if(key STREQUAL "BUILD_INTERFACE")
        continue()
      elseif(key STREQUAL "INSTALL_INTERFACE")
        set(v "${val}")
        if(val AND PROPERTY_DIR AND NOT IS_ABSOLUTE "${val}")
          cmake_path(
            APPEND PROPERTY_DIR "${val}"
            OUTPUT_VARIABLE v
          )
        endif()
      else()
        set(v "${val}")
        if(val AND key)
          get_generator_expresion_args("${key}" subkey VALUE_VAR subval)
          if(subkey STREQUAL "COMPILE_LANGUAGE"
             AND NOT subval IN_LIST ARGS_COMPILE_LANGUAGE)
            set(v)
          elseif(subkey STREQUAL "LINK_LANGUAGE"
                 AND NOT subval IN_LIST ARGS_LINK_LANGUAGE)
            set(v)
          else()
            message(DEBUG "HERE: subkey=${subkey}, subval=${subval}")
          endif()
        endif()
      endif()
    else()
      set(v ${vraw})
    endif()
    if(v AND PROPERTY STREQUAL "LINK_LIBRARIES" AND TARGET ${v})
      if("${v}" IN_LIST ARGS_LOCAL_TARGETS)
        # TODO: Use predicted location?
        set(v)
      else()
        get_target_property(v_location ${v} LOCATION)
        if(v_location STREQUAL "v_location-NOTFOUND")
          set(v)
        else()
          set(v ${v_location})
        endif()
      endif()
    endif()
    if(v AND PROPERTY STREQUAL "LINK_OPTIONS"
       AND v MATCHES "^LINKER:(.+)$")
      # set(v "${CMAKE_MATCH_1}")
      set(v)  # TODO: This may need to be added before the library is linked
    endif()
    if(v AND NOT v IN_LIST ${OUTPUT_VAR})
      list(APPEND ${OUTPUT_VAR} ${v})
    endif()
  endforeach()
  if(ARGS_COMPLETE)
    set(EXTRA_PROPERTIES)
    if(PROPERTY STREQUAL "LINK_OPTIONS")
      list(APPEND EXTRA_PROPERTIES LINK_DIRECTORIES LINK_LIBRARIES)
    elseif(PROPERTY STREQUAL "COMPILE_OPTIONS")
      list(APPEND EXTRA_PROPERTIES INCLUDE_DIRECTORIES)
    endif()
    foreach(extra IN LISTS EXTRA_PROPERTIES)
      get_target_install_property(
        TARGET_${extra} ${TARGET} ${extra} ${RECURSIVE_ARGS}
      )
    endforeach()
    if(PROPERTY STREQUAL "LINK_OPTIONS")
      foreach(dir IN LISTS TARGET_LINK_DIRECTORIES)
        if(NOT "-L${dir}" IN_LIST ${OUTPUT_VAR})
          list(APPEND ${OUTPUT_VAR} "-L${dir}")
        endif()
      endforeach()
      foreach(lib IN LISTS TARGET_LINK_LIBRARIES)
        if(EXISTS ${lib})
          get_library_parts("${lib}" lib)
          if(lib_LIBDIR AND NOT "-L${lib_LIBDIR}" IN_LIST ${OUTPUT_VAR})
            list(APPEND ${OUTPUT_VAR} "-L${lib_LIBDIR}")
          endif()
          if(lib_LIBNAME AND NOT "-l${lib_LIBNAME}" IN_LIST ${OUTPUT_VAR})
            list(APPEND ${OUTPUT_VAR} "-l${lib_LIBNAME}")
          endif()
        endif()
      endforeach()
    elseif(PROPERTY STREQUAL "COMPILE_OPTIONS")
      foreach(inc IN LISTS TARGET_INCLUDE_DIRECTORIES)
        if(NOT "-I${inc}" IN_LIST ${OUTPUT_VAR})
          list(APPEND ${OUTPUT_VAR} "-I${inc}")
        endif()
      endforeach()
    endif()
  endif()
  if(ARGS_RECURSIVE)
    get_target_property(RECURSE_LIBS ${TARGET} INTERFACE_LINK_LIBRARIES)
    if(NOT RECURSE_LIBS STREQUAL "RECURSE_LIBS-NOTFOUND")
      foreach(recurse_target IN LISTS RECURSE_LIBS)
        if(TARGET ${recurse_target})
          get_target_install_property(
            ${OUTPUT_VAR} ${recurse_target} ${PROPERTY} RECURSIVE APPEND
            ${RECURSIVE_ARGS}
          )
        endif()
      endforeach()
    endif()
  endif()
  # message(DEBUG "${PROPERTY} ${TARGET}: ${${OUTPUT_VAR}} (${RAW_OUTPUT})")
  set(${OUTPUT_VAR} ${${OUTPUT_VAR}} PARENT_SCOPE)
endfunction()

function(install_pkgconfig)
  set(oneValueArgs PROJECT_NAME PROJECT_VERSION TEMPLATE
      INSTALL_PREFIX INSTALL_LIBDIR INSTALL_INCLUDEDIR
      INSTALL_PKGCONFIGDIR)
  set(multiValueArgs TARGETS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(ARGS_PROJECT_NAME)
    set(PROJECT_NAME ${ARGS_PROJECT_NAME})
  endif()
  if(ARGS_PROJECT_VERSION)
    set(PROJECT_VERSION ${ARGS_PROJECT_VERSION})
  endif()
  if((NOT ARGS_TARGETS) AND (TARGET ${PROJECT_NAME}))
    list(APPEND ARGS_TARGETS ${PROJECT_NAME})
  endif()
  if(NOT ARGS_TARGETS)
    message(FATAL_ERROR "No targets provided")
  endif()
  if(NOT ARGS_TEMPLATE)
    cmake_path(
      APPEND CMAKE_CURRENT_SOURCE_DIR "${PROJECT_NAME}.pc.in"
      OUTPUT_VARIABLE ARGS_TEMPLATE
    )
  endif()
  set(FILES)
  foreach(TARGET IN LISTS ARGS_TARGETS)
    set(TARGET_COMPILE_LANGUAGE ${${TARGET}_LANGUAGES})
    get_target_property(TARGET_LINK_LANGUAGE ${TARGET} LINKER_LANGUAGE)
    # message(DEBUG "${TARGET}: ${TARGET_COMPILE_LANGUAGE}")
    set(properties COMPILE_OPTIONS LINK_OPTIONS)
    set(TARGET_COMPILE_OPTIONS)
    set(TARGET_LINK_OPTIONS -L${CMAKE_INSTALL_FULL_LIBDIR} -l${TARGET})
    foreach(name IN LISTS properties)
      get_target_install_property(
        TARGET_${name}
        ${TARGET} ${name}
        COMPILE_LANGUAGE ${TARGET_COMPILE_LANGUAGE}
        LINK_LANGUAGE ${TARGET_LINK_LANGUAGE}
        RECURSIVE COMPLETE APPEND
        LOCAL_TARGETS ${ARGS_TARGETS}
      )
    endforeach()
    cmake_path(
      APPEND CMAKE_CURRENT_BINARY_DIR "${TARGET}.pc"
      OUTPUT_VARIABLE dst
    )
    string(REPLACE ";" " " TARGET_LINK_OPTIONS "${TARGET_LINK_OPTIONS}")
    string(REPLACE ";" " " TARGET_COMPILE_OPTIONS "${TARGET_COMPILE_OPTIONS}")
    string(REPLACE ";" " & " TARGET_COMPILE_LANGUAGE "${TARGET_COMPILE_LANGUAGE}")
    message(DEBUG "TARGET_COMPILE_LANGUAGE: ${TARGET_COMPILE_LANGUAGE}")
    foreach(name IN LISTS properties)
      message(DEBUG "${TARGET}-${name}: ${TARGET_${name}}")
    endforeach()
    configure_file(${ARGS_TEMPLATE} ${dst} @ONLY)
    list(APPEND FILES ${dst})
  endforeach()
  # message(FATAL_ERROR "STOP")
  if(NOT ARGS_INSTALL_LIBDIR)
    cmake_path(
      APPEND ARGS_INSTALL_PREFIX ${CMAKE_INSTALL_LIBDIR}
      OUTPUT_VARIABLE ARGS_INSTALL_LIBDIR
    )
  endif()
  if(NOT ARGS_INSTALL_PKGCONFIG_DIR)
    cmake_path(
      APPEND ARGS_INSTALL_LIBDIR pkgconfig
      OUTPUT_VARIABLE ARGS_INSTALL_PKGCONFIG_DIR
    )
  endif()
  install(
    FILES ${FILES}
    DESTINATION ${ARGS_INSTALL_PKGCONFIG_DIR}
    COMPONENT pkgconfig
  )
endfunction()

function(complete_install PROJECT)
  include(GNUInstallDirs)
  set(options DONT_INSTALL_CMAKE_PACKAGING
      DONT_INSTALL_PKGCONFIG_PACKAGING DONT_INSTALL_HEADERS
      DONT_INSTALL_TARGETS DONT_INSTALL_DOCS NESTED_INCLUDEDIR)
  set(oneValueArgs EXPORT VERSION COMPONENT
      CONFIG_TEMPLATE PKGCONFIG_TEMPLATE MACROS_FILE
      INSTALL_PREFIX INSTALL_LIBDIR INSTALL_BINDIR
      INSTALL_INCLUDEDIR INSTALL_CMAKEDIR INSTALL_CMAKEDIR_CONFIG
      INSTALL_DOCDIR
      MODULE_DIR MODULES_INCLUDE_PATTERN MODULES_EXCLUDE_PATTERN
      MODULE_SCRIPT_DIR
      HEADER_DIR DOC_DIR CONFIG_TEMPLATE_DIR)
  set(multiValueArgs TARGETS HEADERS MODULES_INCLUDE MODULES_EXCLUDE
      DOCS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(NOT ARGS_EXPORT)
    set(ARGS_EXPORT ${PROJECT}Targets)
  endif()
  set(CONDA_BUILD "$ENV{CONDA_BUILD}")
  message(DEBUG "CMAKE_INSTALL_PREFIX = ${CMAKE_INSTALL_PREFIX}")
  message(DEBUG "ARGS_INSTALL_PREFIX = ${ARGS_INSTALL_PREFIX}")
  message(DEBUG "CONDA_BUILD = ${CONDA_BUILD}") 
  # if(WIN32 AND (NOT MSVC) AND CONDA_BUILD AND
  #    (NOT ARGS_INSTALL_PREFIX MATCHES ".+Library$"))
  #   cmake_path(
  #     APPEND ARGS_INSTALL_PREFIX Library
  #     OUTPUT_VARIABLE ARGS_INSTALL_PREFIX
  #   )
  # endif()
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
        APPEND ARGS_INSTALL_PREFIX cmake ${PROJECT}
        OUTPUT_VARIABLE ARGS_INSTALL_CMAKEDIR
      )
      # Put config file in non-project directory so cmake can find it
      # on windows
      if(NOT ARGS_INSTALL_CMAKEDIR_CONFIG)
        cmake_path(
          APPEND ARGS_INSTALL_PREFIX cmake
          OUTPUT_VARIABLE ARGS_INSTALL_CMAKEDIR_CONFIG
        )
      endif()
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
    set(HEADER_FILES)
    set(HEADER_DIRS)
    foreach(header IN LISTS ARGS_HEADERS)
      if(IS_DIRECTORY "${header}")
        install(
          DIRECTORY ${header}
          DESTINATION ${ARGS_INSTALL_INCLUDEDIR}
          ${COMPONENT_ARGS}
        )
        list(APPEND HEADER_DIRS "${header}")
      else()
        list(APPEND HEADER_FILES "${header}")
      endif()
    endforeach()
    if(HEADER_FILES)
      install(
        FILES ${HEADER_FILES}
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
  # pkg-config file
  ###################################

  if(NOT ARGS_DONT_INSTALL_PKGCONFIG_PACKAGING)
    install_pkgconfig(
      PROJECT_NAME ${PROJECT}
      TARGETS ${ARGS_TARGETS}
      TEMPLATE ${ARGS_PKGCONFIG_TEMPLATE}
      INSTALL_PREFIX ${ARGS_INSTALL_PREFIX}
      INSTALL_LIBDIR ${ARGS_INSTALL_LIBDIR}
      INSTALL_INCLUDEDIR ${ARGS_INSTALL_INCLUDEDIR}
    )
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
    if(ARGS_MODULE_DIR AND NOT ARGS_MODULE_SCRIPT_DIR)
      cmake_path(
        APPEND ARGS_MODULE_DIR scripts
        OUTPUT_VARIABLE ARGS_MODULE_SCRIPT_DIR
      )
      if(NOT EXISTS ${ARGS_MODULE_SCRIPT_DIR})
        set(ARGS_MODULE_SCRIPT_DIR)
      endif()
    endif()
    if(NOT ARGS_CONFIG_TEMPLATE)
      set(CONFIG_TEMPLATE_BASE "${PROJECT}Config.cmake.in")
      if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/${CONFIG_TEMPLATE_BASE}")
        cmake_path(
          APPEND CMAKE_CURRENT_LIST_DIR ${CONFIG_TEMPLATE_BASE}
          OUTPUT_VARIABLE ARGS_CONFIG_TEMPLATE
        )
      elseif(ARGS_MODULE_DIR AND EXISTS "${ARGS_MODULE_DIR}/${CONFIG_TEMPLATE_BASE}")
        cmake_path(
          APPEND ARGS_MODULE_DIR ${CONFIG_TEMPLATE_BASE}
          OUTPUT_VARIABLE ARGS_CONFIG_TEMPLATE
        )
      endif()
    endif()
    if(ARGS_CONFIG_TEMPLATE)
      if(ARGS_INSTALL_CMAKEDIR_CONFIG)
        cmake_path(
          RELATIVE_PATH ARGS_INSTALL_CMAKEDIR
          BASE_DIRECTORY "${ARGS_INSTALL_CMAKEDIR_CONFIG}"
          OUTPUT_VARIABLE YGG_CMAKE_MODULE_DIR
        )
      else()
        set(YGG_CMAKE_MODULE_DIR)
      endif()
      configure_file(
        ${ARGS_CONFIG_TEMPLATE}
        ${PROJECT}Config.cmake @ONLY
      )
      set(CMAKE_CONFIG_FILE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT}Config.cmake" )
      if(ARGS_INSTALL_CMAKEDIR_CONFIG)
        # CMake does not search %PREFIX%/lib/cmake/<PROJECT>/ on Windows
        # so the config file should be placed somewhere it can find and
        # then pointed to the other package cmake modules
        install(
          FILES ${CMAKE_CONFIG_FILE}
          DESTINATION ${ARGS_INSTALL_CMAKEDIR_CONFIG}
          ${COMPONENT_ARGS}
        )
      else()
        list(APPEND ARGS_MODULES_INCLUDE "${CMAKE_CONFIG_FILE}")
      endif()
    endif()
    if(NOT ARGS_MACROS_FILE)
      set(MACROS_FILE_BASE "${PROJECT}Macros.cmake")
      if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/${MACROS_FILE_BASE}")
        cmake_path(
          APPEND CMAKE_CURRENT_LIST_DIR ${MACROS_FILE_BASE}
          OUTPUT_VARIABLE ARGS_MACROS_FILE
        )
      elseif(ARGS_MODULE_DIR AND EXISTS "${ARGS_MODULE_DIR}/${MACROS_FILE_BASE}")
        cmake_path(
          APPEND ARGS_MODULE_DIR ${MACROS_FILE_BASE}
          OUTPUT_VARIABLE ARGS_MACROS_FILE
        )
      endif()
    endif()
    if(ARGS_MACROS_FILE)
      list(APPEND ARGS_MODULES_INCLUDE "${ARGS_MACROS_FILE}")
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
    if(ARGS_MODULE_SCRIPT_DIR)
      install(
        DIRECTORY ${ARGS_MODULE_SCRIPT_DIR}
        DESTINATION ${ARGS_INSTALL_CMAKEDIR}
        FILES_MATCHING REGEX ".*\\.py$"
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
