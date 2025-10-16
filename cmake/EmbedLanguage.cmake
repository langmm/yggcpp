function(embed_language LANGUAGE)
  include(AddYggInterface)
  set(options DISABLE_BY_DEFAULT VERBOSE)
  set(oneValueArgs FIND_METHOD DEPENDENCY)
  set(multiValueArgs FIND_ARGS LIBRARIES LIBRARIES_Python
      LIBRARY_DIRS INCLUDE_DIRS DEFINITIONS DEFINITIONS_MISSING
      DEPENDENCY_PROPERTIES SEARCH_ARGS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  check_no_unparsed(ARGS)
  set(DEFAULT_ENABLED ON)
  if (ARGS_DISABLE_BY_DEFAULT)
    set(DEFAULT_ENABLED OFF)
  endif()
  option(YGG_EMBED_${LANGUAGE} "Build interfaces to support ${LANGUAGE} embedding" ${DEFAULT_ENABLED})
  option(YGG_EMBED_${LANGUAGE}_REQUIRED "Require support for ${LANGUAGE} embedding" OFF)
  set_default(ARGS_DEPENDENCY ${LANGUAGE})
  string(TOUPPER ${LANGUAGE} LANGUAGE_UPPER)
  list(APPEND ARGS_DEFINITIONS "-DYGG_EMBED_${LANGUAGE_UPPER}")
  list(APPEND ARGS_DEFINITIONS_MISSING
       "-DYGGDRASIL_DISABLE_EMBEDDED_${LANGUAGE_UPPER}")
  if (LANGUAGE STREQUAL "Python")
    list(APPEND ARGS_DEFINITIONS_MISSING
         -DYGGDRASIL_DISABLE_PYTHON_C_API)
  endif()
  if(YGG_EMBED_${LANGUAGE})
    if (ARGS_FIND_METHOD)
      message(FATAL_ERROR "Deprecated FIND_METHOD")
      # cmake_language(CALL ${ARGS_FIND_METHOD} ${ARGS_FIND_ARGS})
      # if(NOT ${ARGS_DEPENDENCY}_FOUND)
      #   message(STATUS "${LANGUAGE} not found, embedding will be disabled")
      #   set(YGG_EMBED_${LANGUAGE} OFF)
      # endif()
    else()
      collect_arguments(
        FIND_ARGS ARGS "${options}"
        LIBRARIES LIBRARIES_Python
        LIBRARY_DIRS INCLUDE_DIRS DEFINITIONS DEFINITIONS_MISSING
        DEPENDENCY_PROPERTIES SEARCH_ARGS
      )
      add_yggdrasil_dependency(
        ${ARGS_DEPENDENCY} ${FIND_ARGS}
        OPTIONS_MISSING "YGG_EMBED_${LANGUAGE}"
        DOC_MISSING "\"${ARGS_DEPENDENCY}\" not found, embedding of \"${LANGUAGE}\" will be disabled"
      )
    endif()
    
    propagate_cmake_variables_prefix(
      "${ARGS_DEPENDENCY}" ${ARGS_DEPENDENCY_PROPERTIES}
    )
    if(ARGS_VERBOSE)
      dump_cmake_variables(PREFIX "${ARGS_DEPENDENCY}" VERBOSE)
    endif()
  else()
    message(STATUS "${LANGUAGE} embedding disabled")
    list(APPEND YGG_INSTALL_DEFS ${ARGS_DEFINITIONS_MISSING})
  endif()
  if(YGG_EMBED_${LANGUAGE})
    list(APPEND YGG_EMBEDDED_LANGUAGES_AVAILABLE ${LANGUAGE})
  elseif(YGG_EMBED_${LANGUAGE}_REQUIRED)
    message(WARNING "Embedded language \"${LANGUAGE}\" required, but not enabled")
  endif()
  list(APPEND YGG_EMBEDDED_LANGUAGES_SUPPORTED ${LANGUAGE})
  propagate_cmake_variables(
    YGG_EMBED_${LANGUAGE}
    YGG_EMBED_${LANGUAGE}_REQUIRED
    YGG_INSTALL_DEPS
    YGG_INSTALL_DEFS
    YGG_INSTALL_CONFIG
    DEPS_LIBRARIES
    DEPS_LIBRARIES_Python
    DEPS_LIBRARY_DIRS
    DEPS_INCLUDE_DIRS
    YGG_EMBEDDED_LANGUAGES_SUPPORTED
    YGG_EMBEDDED_LANGUAGES_AVAILABLE
  )
endfunction()