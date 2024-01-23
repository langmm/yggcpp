if(YGG_PYTHON_LIBRARY_WRAP)
    set(YGG_TARGET_PY _YggInterface)
else()
    set(YGG_TARGET_PY YggInterface)
endif()
set(SOURCES_PY ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface.cpp)
if (NOT YGG_LINK_PYTHON_TO_CPP)
    set(SOURCES_PY ${SOURCES_PY} ${SOURCES_CPP})
endif()
cmake_path(GET CMAKE_CURRENT_SOURCE_DIR PARENT_PATH PARENT_SOURCE_DIR)
cmake_path(GET CMAKE_CURRENT_BINARY_DIR PARENT_PATH PARENT_BINARY_DIR)

if (NOT SKBUILD)
  if (BUILD_PYTHON_LIBRARY AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
    set(PYTHON_COPY_ARGS ENABLE_OMP USING_IPC ENABLE_OMP ENABLE_MPI
        ENABLE_THREADS YGG_ENABLE_COVERAGE
	YGG_SKIP_VALGRIND_TESTS YGG_BUILD_ASAN YGG_BUILD_UBSAN
	YGG_ENABLE_INSTRUMENTATION_OPT WRAP_RAPIDJSON_FOR_DLL
	YGG_DEBUG_LEVEL RAPIDJSON_INCLUDE_DIRS)
    foreach(opt IN LISTS PYTHON_COPY_ARGS)
      list(APPEND PYTHON_CMAKE_ARGS "-D${opt}=${${opt}}")
    endforeach()
    list(APPEND PYTHON_CMAKE_ARGS
         -DBUILD_CPP_LIBRARY=OFF -DBUILD_FORTRAN_LIBRARY=OFF)
    if (CMAKE_PREFIX_PATH)
      list(APPEND PYTHON_BUILD_ARGS
        --config-settings=cmake.define.CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH})
    endif()
    if (${PYTHON_PREFIX}_ROOT_DIR)
      list(APPEND PYTHON_BUILD_ARGS
        --config-settings=cmake.define.Python3_ROOT_DIR=${${PYTHON_PREFIX}_ROOT_DIR}
        --config-settings=cmake.define.Python_ROOT_DIR=${${PYTHON_PREFIX}_ROOT_DIR})
    endif()
    message(STATUS "Building Python with CMAKE_ARGS = ${PYTHON_CMAKE_ARGS}")

    include(BuildTools)
    set(DEPENDS_PY ${SOURCES_PY} ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface/__init__.py)
    message(STATUS "PYTHON_OUTPUT = ${${PYTHON_PREFIX}_SITELIB}/YggInterface/__init__.py")
    if (EXISTS "${${PYTHON_PREFIX}_SITELIB}/YggInterface/__init__.py")
      message(STATUS "PYTHON_OUTPUT EXISTS")
    else()
      message(STATUS "PYTHON_OUTPUT DOES NOT EXIST")
    endif()
    add_custom_command_env(
      YggInterface_python
      COMMAND ${${PYTHON_PREFIX}_EXECUTABLE} -m pip install ${PARENT_SOURCE_DIR} -v ${PYTHON_BUILD_ARGS}
      KEYPREFIX "ENVK_"
      ENV ENVK_CMAKE_ARGS ${PYTHON_CMAKE_ARGS} END_ENV
      COMMENT "build Python via pip"
      DEPENDS ${DEPENDS_PY})
      # OUTPUT ${${PYTHON_PREFIX}_SITELIB}/YggInterface/__init__.py)
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
if(YGG_LINK_PYTHON_TO_CPP)
    target_link_libraries(${YGG_TARGET_PY} PUBLIC ${YGG_TARGET_CPP})
else()
    target_link_libraries(${YGG_TARGET_PY} PUBLIC
                          ${DEPS_LIBRARIES} ${DEPS_PUBLIC_CXX_LIBS})
    target_link_libraries(${YGG_TARGET_PY} PUBLIC
                          ${DEPS_LIBRARIES_PRIVATE})
endif()
target_compile_options(${YGG_TARGET_PY} PUBLIC
	  ${YGG_INSTALL_DEFS} -DRAPIDJSON_YGGDRASIL_PYTHON)
if(YGG_PYTHON_LIBRARY_WRAP)
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

set(YGG_CPP_LIBRARY_DEST ".")

if(NOT ${PYTHON_PREFIX}_INSTALL_DIR)
  set(${PYTHON_PREFIX}_INSTALL_DIR ".")
endif()

if(YGG_LINK_PYTHON_TO_CPP)
  install(TARGETS ${YGG_TARGET_CPP}
          LIBRARY DESTINATION ${YGG_CPP_LIBRARY_DEST}
	  PUBLIC_HEADER DESTINATION ${YGG_CPP_LIBRARY_DEST}
	  RUNTIME DESTINATION ${YGG_CPP_LIBRARY_DEST})
  install(TARGETS ${YGG_TARGET_PY}
  	  LIBRARY DESTINATION ${${PYTHON_PREFIX}_INSTALL_DIR})
else()
  target_compile_definitions(
    ${YGG_TARGET_PY} PUBLIC ${YGG_INSTALL_DEFS})
  target_compile_options(
    ${YGG_TARGET_PY} PUBLIC ${DEPS_PUBLIC_COMPILE_FLAGS}
    ${DEPS_PUBLIC_CXX_COMPILE_FLAGS})
  install(TARGETS ${YGG_TARGET_PY}
          LIBRARY DESTINATION ${${PYTHON_PREFIX}_INSTALL_DIR}
	  RUNTIME DESTINATION ${${PYTHON_PREFIX}_INSTALL_DIR})
endif()
if(APPLE)
  set_target_properties(
    ${YGG_TARGET_PY} PROPERTIES INSTALL_RPATH
    "@loader_path/${YGG_CPP_LIBRARY_DEST}")
else()
  set_target_properties(
    ${YGG_TARGET_PY} PROPERTIES INSTALL_RPATH
    "$ORIGIN/${YGG_CPP_LIBRARY_DEST}")
endif()
