function(generate_export_header_yggdrasil target)
  include(GenerateExportHeader)
  set(oneValueArgs BASE_NAME EXPORT_MACRO_NAME
      EXPORT_FILE_NAME)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_BASE_NAME)
    set(ARGS_BASE_NAME YGG_API)
  endif()
  if (NOT ARGS_EXPORT_MACRO_NAME)
    set(ARGS_EXPORT_MACRO_NAME YGG_API)
  endif()
  if (NOT ARGS_EXPORT_FILE_NAME)
    set(ARGS_EXPORT_FILE_NAME YggInterface_export.h)
  endif()
  generate_export_header(
    ${target}
    BASE_NAME ${ARGS_BASE_NAME}
    EXPORT_MACRO_NAME ${ARGS_EXPORT_MACRO_NAME}
    EXPORT_FILE_NAME ${ARGS_EXPORT_FILE_NAME}
    ${ARGS_UNPARSED_ARGUMENTS})
  list(APPEND YGG_INSTALL_INCLUDES
       ${CMAKE_CURRENT_BINARY_DIR}/${ARGS_EXPORT_FILE_NAME})
  set(YGG_INSTALL_INCLUDES ${YGG_INSTALL_INCLUDES} PARENT_SCOPE)
  target_include_directories(
       ${target} PUBLIC
       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)
endfunction()