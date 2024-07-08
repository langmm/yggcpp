function(add_dynamic_dependencies TARGET)
  add_dependencies(${TARGET} ${ARGN})
  cmake_path(GET CMAKE_CURRENT_BINARY_DIR PARENT_PATH PARENT_BINARY_DIR)
  foreach(lib ${ARGN})
    add_custom_command(
      TARGET ${TARGET}
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> ${PARENT_BINARY_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND_EXPAND_LISTS
    )
  endforeach()
endfunction()