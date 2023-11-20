function(show_runtimes target)
  if (WIN32)
    add_custom_command(
      TARGET ${target}
      POST_BUILD
      COMMAND echo "TARGET_RUNTIME_DLLS(${target}) = $<TARGET_RUNTIME_DLLS:${target}>"
      )
    if (MSVC)
      add_custom_command(
        TARGET ${target}
	POST_BUILD
	COMMAND dumpbin /dependents $<TARGET_FILE:${target}>
	COMMAND_EXPAND_LISTS
	)
    else()
      add_custom_command(
        TARGET ${target}
	POST_BUILD
	COMMAND objdump -x $<TARGET_FILE:${target}> | grep "DLL Name"
	COMMAND_EXPAND_LISTS
	)
    endif()
  endif()
endfunction()