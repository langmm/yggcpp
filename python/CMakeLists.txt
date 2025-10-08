if(YGG_Python_LIBRARY_WRAP)
    set(YGG_TARGET_PY _YggInterface)
else()
    set(YGG_TARGET_PY YggInterface)
endif()
set(SOURCES_PY ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface.cpp)
if (NOT YGG_LINK_Python_TO_CXX)
    set(SOURCES_PY ${SOURCES_PY} ${SOURCES_CXX})
endif()
cmake_path(GET CMAKE_CURRENT_SOURCE_DIR PARENT_PATH PARENT_SOURCE_DIR)
cmake_path(GET CMAKE_CURRENT_BINARY_DIR PARENT_PATH PARENT_BINARY_DIR)

if (NOT SKBUILD)
  if (BUILD_Python_LIBRARY AND NOT YGGDRASIL_DISABLE_Python_C_API)
    set(Python_COPY_ARGS
        DEFAULT_COMM YGG_ENABLE_COVERAGE
	YGG_SKIP_VALGRIND_TESTS YGG_BUILD_ASAN YGG_BUILD_UBSAN
	YGG_ENABLE_INSTRUMENTATION_OPT WRAP_RAPIDJSON_FOR_DLL
	YGG_DEBUG_LEVEL RAPIDJSON_INCLUDE_DIRS)
    foreach(lang ${YGG_EMBEDDED_LANGUAGES})
      list(APPEND Python_COPY_ARGS YGG_EMBED_${lang})
    endforeach()
    foreach(opt ${YGG_OPTIONS_SUPPORTED})
      list(APPEND Python_COPY_ARGS ENABLE_${opt})
    endforeach()
    foreach(opt IN LISTS Python_COPY_ARGS)
      list(APPEND Python_CMAKE_ARGS "-D${opt}=${${opt}}")
    endforeach()
    list(APPEND Python_CMAKE_ARGS
         -DBUILD_CXX_LIBRARY=OFF -DBUILD_Fortran_LIBRARY=OFF)
    if (CMAKE_PREFIX_PATH)
      list(APPEND Python_BUILD_ARGS
        --config-settings=cmake.define.CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH})
    endif()
    if (${Python_PREFIX}_ROOT_DIR)
      list(APPEND Python_BUILD_ARGS
        --config-settings=cmake.define.Python3_ROOT_DIR=${${Python_PREFIX}_ROOT_DIR}
        --config-settings=cmake.define.Python_ROOT_DIR=${${Python_PREFIX}_ROOT_DIR})
    endif()
    message(STATUS "Building Python with CMAKE_ARGS = ${Python_CMAKE_ARGS}")

    include(BuildTools)
    set(DEPENDS_PY ${SOURCES_PY} ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface/__init__.py)
    message(STATUS "Python_OUTPUT = ${${Python_PREFIX}_SITELIB}/YggInterface/__init__.py")
    if (EXISTS "${${Python_PREFIX}_SITELIB}/YggInterface/__init__.py")
      message(STATUS "Python_OUTPUT EXISTS")
    else()
      message(STATUS "Python_OUTPUT DOES NOT EXIST")
    endif()
    add_custom_command_env(
      YggInterface_python
      COMMAND ${${Python_PREFIX}_EXECUTABLE} -m pip install ${PARENT_SOURCE_DIR} -v ${Python_BUILD_ARGS}
      KEYPREFIX "ENVK_"
      ENV ENVK_CMAKE_ARGS ${Python_CMAKE_ARGS} END_ENV
      COMMENT "build Python via pip"
      DEPENDS ${DEPENDS_PY})
      # OUTPUT ${${Python_PREFIX}_SITELIB}/YggInterface/__init__.py)
  endif()
  return()
endif()

message(STATUS "Building Python bindings=Yes")

if (NOT SKBUILD STREQUAL "2")
  message(FATAL_ERROR "Python interface must be built using scikit-build-core")
endif()

add_library_python(${YGG_TARGET_PY} MODULE ${SOURCES_PY})
target_include_directories(
          ${YGG_TARGET_PY} PUBLIC
          ${DEPS_INCLUDE_DIRS}
	  ${PARENT_SOURCE_DIR}/cpp/include
	  ${RAPIDJSON_INCLUDE_DIRS})
if(YGG_LINK_Python_TO_CXX)
    target_link_libraries(${YGG_TARGET_PY} PUBLIC ${YGG_TARGET_CXX})
else()
    target_link_libraries(${YGG_TARGET_PY} PUBLIC
                          ${DEPS_LIBRARIES} ${DEPS_PUBLIC_CXX_LIBS})
    target_link_libraries(${YGG_TARGET_PY} PUBLIC
                          ${DEPS_LIBRARIES_PRIVATE})
endif()
target_compile_options(${YGG_TARGET_PY} PUBLIC
	  ${YGG_INSTALL_DEFS} -DRAPIDJSON_YGGDRASIL_PYTHON)
if(YGG_Python_LIBRARY_WRAP)
    target_compile_options(${YGG_TARGET_PY} PRIVATE
              -DYGG_PYTHON_LIBRARY_WRAP)
endif()
if (ASAN_COMPILE_FLAGS)
    target_compile_options(
      ${YGG_TARGET_PY} PUBLIC ${ASAN_COMPILE_FLAGS})
endif()
if (ASAN_LINK_FLAGS)
    target_link_options(
      ${YGG_TARGET_PY} PUBLIC ${ASAN_LINK_FLAGS})
endif()

if (NOT YGG_LINK_Python_TO_CXX)
    if (MINGW)
        list(APPEND EXPORT_HEADER_ARGS EMPTY)
    endif()
    include(GenerateExportHeaderYggdrasil)
    generate_export_header_yggdrasil(
        ${YGG_TARGET_PY} ${EXPORT_HEADER_ARGS})
    set(YGG_INSTALL_INCLUDES ${YGG_INSTALL_INCLUDES} PARENT_SCOPE)
endif()

if(NOT ${Python_PREFIX}_INSTALL_DIR)
  set(${Python_PREFIX}_INSTALL_DIR ".")
endif()

set(YGG_CXX_LIBRARY_DEST "${${Python_PREFIX}_INSTALL_DIR}")

if(YGG_LINK_Python_TO_CXX)
  install(TARGETS ${YGG_TARGET_CXX}
          LIBRARY DESTINATION ${YGG_CXX_LIBRARY_DEST}
	  PUBLIC_HEADER DESTINATION ${YGG_CXX_LIBRARY_DEST}
	  RUNTIME DESTINATION ${YGG_CXX_LIBRARY_DEST})
  install(TARGETS ${YGG_TARGET_PY}
  	  LIBRARY DESTINATION ${${Python_PREFIX}_INSTALL_DIR})
else()
  target_compile_definitions(
    ${YGG_TARGET_PY} PUBLIC ${YGG_INSTALL_DEFS})
  target_compile_options(
    ${YGG_TARGET_PY} PUBLIC ${DEPS_PUBLIC_COMPILE_FLAGS}
    ${DEPS_PUBLIC_CXX_COMPILE_FLAGS})
  install(TARGETS ${YGG_TARGET_PY}
          LIBRARY DESTINATION ${${Python_PREFIX}_INSTALL_DIR}
	  RUNTIME DESTINATION ${${Python_PREFIX}_INSTALL_DIR})
endif()

include(AddYggInterface)
add_library_dirs_to_rpath(
  ${YGG_TARGET_PY} DESTINATION ${YGG_CXX_LIBRARY_DEST}
)

include(CheckDLL)
show_runtimes(${YGG_TARGET_PY})
