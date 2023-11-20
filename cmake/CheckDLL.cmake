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