include(GeneralTools)

function(copy_required_runtimes TARGET)
  set(oneValueArgs DESTINATION DESTINATION_TARGET)
  set(multiValueArgs DEPENDENCIES)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (NOT ARGS_DESTINATION_TARGET)
    set(ARGS_DESTINATION_TARGET ${TARGET})
  endif()
  if (NOT ARGS_DESTINATION)
    set(ARGS_DESTINATION "$<TARGET_FILE_DIR:${ARGS_DESTINATION_TARGET}>")
  endif()
  if (WIN32)
    # if (CONDA_PREFIX)
      foreach(lib ${ARGS_DEPENDENCIES})
        add_custom_command(
          TARGET ${ARGS_DESTINATION_TARGET}
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${lib}> ${ARGS_DESTINATION}
          COMMAND_EXPAND_LISTS
        )
      endforeach()
    # else()
    #   add_custom_command(
    #       TARGET ${ARGS_DESTINATION_TARGET}
    #       POST_BUILD
    #       COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_RUNTIME_DLLS:${TARGET}> ${ARGS_DESTINATION}
    #       COMMAND_EXPAND_LISTS
    #   )
    # endif()
  endif()
endfunction()

function(check_object file after_target)
  set(oneValueArgs TOOLNAME)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(ADD_ARGS --mode=objects)
  if(ARGS_TOOLNAME)
    list(APPEND ADD_ARGS "--tool=${ARGS_TOOLNAME}")
  endif()
  add_custom_command(
    TARGET ${after_target}
    POST_BUILD
    COMMAND python
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/scripts/inspect_runtime_dependencies.py
    ${file} ${ADD_ARGS}
    COMMAND_EXPAND_LISTS
  )
endfunction()

function(show_runtimes target)
  set(options IMPORTED ONLY_MISSING SHOW_ALL)
  set(oneValueArgs AFTER_TARGET)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if(YGG_INSPECT_TARGETS_ONLY_MISSING AND NOT ARGS_SHOW_ALL)
    set(ARGS_ONLY_MISSING ON)
  endif()
  inspect_target(${target})
  if (ARGS_AFTER_TARGET)
    set(after_target ${ARGS_AFTER_TARGET})
  else()
    set(after_target ${target})
  endif()
  set(TOOLARGS --recurse)
  if(ARGS_ONLY_MISSING)
    list(APPEND TOOLARGS --exclude-methods ANY)
  endif()
  set(TOOLNAME)
  if (WIN32)
    if (NOT ARGS_AFTER_TARGET)
      add_custom_command(
        TARGET ${after_target}
        POST_BUILD
        COMMAND echo "TARGET_RUNTIME_DLLS(${target}) = $<TARGET_RUNTIME_DLLS:${target}>"
        )
    endif()
    if (MSVC)
      set(TOOLNAME dumpbin)
      add_custom_command(
        TARGET ${after_target}
	POST_BUILD
	COMMAND dumpbin /dependents $<TARGET_FILE:${target}>
	COMMAND_EXPAND_LISTS
	)
    else()
      set(TOOLNAME objdump)
      add_custom_command(
        TARGET ${after_target}
	POST_BUILD
	COMMAND objdump -x $<TARGET_FILE:${target}> | grep "DLL Name"
	COMMAND_EXPAND_LISTS
	)
    endif()
  elseif(APPLE)
    set(TOOLNAME otool)
    add_custom_command(
      TARGET ${after_target}
      POST_BUILD
      # COMMAND dyldinfo -dylibs $<TARGET_FILE:${target}>
      COMMAND otool -L $<TARGET_FILE:${target}>
      COMMAND_EXPAND_LISTS
    )
  else()
    set(TOOLNAME ldd)
    add_custom_command(
      TARGET ${after_target}
      POST_BUILD
      COMMAND ldd $<TARGET_FILE:${target}>
      COMMAND_EXPAND_LISTS
    )
  endif()
  if (WIN32)
    add_custom_command(
      TARGET ${after_target}
      POST_BUILD
      COMMAND python
      ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/scripts/inspect_runtime_dependencies.py
      $<TARGET_FILE:${target}> --tool ${TOOLNAME} --cmake-runtimes $<TARGET_RUNTIME_DLLS:${target}> ${TOOLARGS}
      COMMAND_EXPAND_LISTS
    )
  else()
    add_custom_command(
      TARGET ${after_target}
      POST_BUILD
      COMMAND python
      ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/scripts/inspect_runtime_dependencies.py
      $<TARGET_FILE:${target}> --tool=${TOOLNAME} ${TOOLARGS}
      COMMAND_EXPAND_LISTS
    )
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
