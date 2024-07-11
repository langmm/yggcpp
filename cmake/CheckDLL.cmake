function(copy_required_runtimes TARGET)
  set(multiValueArgs DEPENDENCIES DESTINATION)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_DESTINATION)
    set(ARGS_DESTINATION "$<TARGET_FILE_DIR:${TARGET}>")
  endif()
  message(STATUS "TARGET = ${TARGET}")
  message(STATUS "ARGS_DEPENDENCIES = ${ARGS_DEPENDENCIES}")
  message(STATUS "ARGS_DESTINATION = ${ARGS_DESTINATION}")
  if (WIN32)
    if (CONDA_PREFIX)
      foreach(lib ${ARGS_DEPENDENCIES})
        add_custom_command(
          TARGET ${TARGET}
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> ${ARGS_DESTINATION}
          COMMAND_EXPAND_LISTS
        )
      endforeach()
    else()
      add_custom_command(
          TARGET ${TARGET}
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_RUNTIME_DLLS:${TARGET}> ${ARGS_DESTINATION}
          COMMAND_EXPAND_LISTS
      )
    endif()
  endif()
endfunction()

function(show_runtimes target)
  set(options IMPORTED)
  set(oneValueArgs AFTER_TARGET)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_AFTER_TARGET)
    set(after_target ${ARGS_AFTER_TARGET})
  else()
    set(after_target ${target})
  endif()
  if (WIN32)
    if (NOT ARGS_AFTER_TARGET)
      add_custom_command(
        TARGET ${after_target}
        POST_BUILD
        COMMAND echo "TARGET_RUNTIME_DLLS(${target}) = $<TARGET_RUNTIME_DLLS:${target}>"
        )
    endif()
    if (MSVC)
      add_custom_command(
        TARGET ${after_target}
	POST_BUILD
	COMMAND dumpbin /dependents $<TARGET_FILE:${target}>
	COMMAND_EXPAND_LISTS
	)
    else()
      add_custom_command(
        TARGET ${after_target}
	POST_BUILD
	COMMAND objdump -x $<TARGET_FILE:${target}> | grep "DLL Name"
	COMMAND_EXPAND_LISTS
	)
    endif()
  endif()
endfunction()
function(show_symbols target)
  set(oneValueArgs AFTER_TARGET)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (ARGS_AFTER_TARGET)
    set(after_target ${ARGS_AFTER_TARGET})
  else()
    set(after_target ${target})
  endif()
  if (MSVC)
    add_custom_command(
        TARGET ${after_target}
	POST_BUILD
	COMMAND dumpbin /exports $<TARGET_FILE:${target}>
	COMMAND_EXPAND_LISTS
	)
  else()
    add_custom_command(
        TARGET ${after_target}
	POST_BUILD
	COMMAND nm $<TARGET_FILE:${target}>
	COMMAND_EXPAND_LISTS
	)
  endif()
endfunction()
