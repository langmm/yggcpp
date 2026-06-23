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
    set(YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER ON)
    include(GeneralTools)
    include(BuildTools)
    ygginterface_options(Python_COPY_ARGS)
    list(
      APPEND Python_COPY_ARGS
      YGG_SKIP_VALGRIND_TESTS
      CMAKE_VERBOSE_MAKEFILE
      CMAKE_MESSAGE_LOG_LEVEL
    )
    foreach(lang ${YGG_EMBEDDED_LANGUAGES})
      list(APPEND Python_COPY_ARGS YGG_EMBED_${lang})
    endforeach()
    foreach(opt ${YGG_OPTIONS_SUPPORTED})
      list(APPEND Python_COPY_ARGS ENABLE_${opt})
    endforeach()
    cmakevars2cmakecliargs(Python_CMAKE_ARGS ${Python_COPY_ARGS})
    list(APPEND Python_CMAKE_ARGS
         -DBUILD_CXX_LIBRARY=OFF -DBUILD_Fortran_LIBRARY=OFF)
    if (CMAKE_PREFIX_PATH)
      set(CMAKE_PREFIX_PATH_SAFE "${CMAKE_PREFIX_PATH}")
      if(WIN32)
        string(REPLACE "\\" "/" CMAKE_PREFIX_PATH_SAFE "${CMAKE_PREFIX_PATH}")
        # 24 leaves 3 backslashes
        # 21 leaves 2 backslashes
        escape_char_repalcement(";" CMAKE_PREFIX_PATH_SAFE LEVEL 19)
        # string(REPLACE ";" "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\;" CMAKE_PREFIX_PATH_SAFE "${CMAKE_PREFIX_PATH_SAFE}")
      endif()
      list(APPEND Python_BUILD_ARGS
        "--config-settings=cmake.define.CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH_SAFE}")
      message(DEBUG "CMAKE_PREFIX_PATH_SAFE = ${CMAKE_PREFIX_PATH_SAFE}")
    endif()
    if (Python3_ROOT_DIR)
      set(Python3_ROOT_DIR_SAFE "${Python3_ROOT_DIR}")
      if(WIN32)
        string(REPLACE "\\" "/" Python3_ROOT_DIR_SAFE "${Python3_ROOT_DIR}")
      endif()
      list(APPEND Python_BUILD_ARGS
        "--config-settings=cmake.define.Python3_ROOT_DIR=${Python3_ROOT_DIR_SAFE}")
    endif()
    message(STATUS "Building Python with CMAKE_ARGS = ${Python_CMAKE_ARGS}")

    set(DEPENDS_PY ${SOURCES_PY} ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface/__init__.py)
    message(DEBUG "Python_OUTPUT = ${Python3_SITELIB}/YggInterface/__init__.py")
    if (EXISTS "${Python3_SITELIB}/YggInterface/__init__.py")
      message(DEBUG "Python_OUTPUT EXISTS")
    else()
      message(DEBUG "Python_OUTPUT DOES NOT EXIST")
    endif()
    message(DEBUG "Python_BUILD_ARGS = ${Python_BUILD_ARGS}")
    add_custom_command_env(
      YggInterface_python
      COMMAND ${Python3_EXECUTABLE} -m pip install ${PARENT_SOURCE_DIR} -v ${Python_BUILD_ARGS}
      KEYPREFIX "ENVK_"
      ENV ENVK_CMAKE_ARGS ${Python_CMAKE_ARGS} END_ENV
      COMMENT "build Python via pip"
      DEPENDS ${DEPENDS_PY}
      # OUTPUT ${Python3_SITELIB}/YggInterface/__init__.py
    )
  endif()
  return()
endif()

message(STATUS "Building Python bindings=Yes")

if (NOT SKBUILD STREQUAL "2")
  message(FATAL_ERROR "Python interface must be built using scikit-build-core")
endif()

Python3_add_library(${YGG_TARGET_PY} MODULE ${SOURCES_PY})
target_include_directories(
          ${YGG_TARGET_PY} PUBLIC
          ${DEPS_INCLUDE_DIRS}
	  ${PARENT_SOURCE_DIR}/cpp/include)
if(YGG_LINK_Python_TO_CXX)
    target_link_libraries(${YGG_TARGET_PY} PUBLIC ${YGG_TARGET_CXX})
else()
    target_link_libraries(
      ${YGG_TARGET_PY} PUBLIC
      ${DEPS_LIBRARIES} ${DEPS_PUBLIC_CXX_LIBS}
    )
    target_link_libraries(
      ${YGG_TARGET_PY} PUBLIC
      ${DEPS_LIBRARIES_PRIVATE}
    )
    target_include_directories(
       ${YGG_TARGET_PY} PUBLIC ${DEPS_INCLUDE_DIRS}
    )
    target_compile_definitions(
      ${YGG_TARGET_PY} PUBLIC ${YGG_INSTALL_DEFS}
    )
    target_compile_options(
      ${YGG_TARGET_PY} PUBLIC
      ${DEPS_PUBLIC_COMPILE_FLAGS} ${DEPS_PUBLIC_CXX_COMPILE_FLAGS}
    )
    target_link_options(
      ${YGG_TARGET_PY} PUBLIC
      ${DEPS_PUBLIC_LINK_FLAGS} ${DEPS_PUBLIC_CXX_LINK_FLAGS}
    )
endif()
if(NOT YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER)
  message(FATAL_ERROR "YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER not set")
endif()
if(YGG_Python_LIBRARY_WRAP)
    target_compile_options(
      ${YGG_TARGET_PY} PRIVATE -DYGG_PYTHON_LIBRARY_WRAP
    )
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

if(NOT Python3_INSTALL_DIR)
  set(Python3_INSTALL_DIR ".")
endif()

set(YGG_CXX_LIBRARY_DEST "${Python3_INSTALL_DIR}")

if(YGG_LINK_Python_TO_CXX)
  install(TARGETS ${YGG_TARGET_CXX}
          LIBRARY DESTINATION ${YGG_CXX_LIBRARY_DEST}
	  PUBLIC_HEADER DESTINATION ${YGG_CXX_LIBRARY_DEST}
	  RUNTIME DESTINATION ${YGG_CXX_LIBRARY_DEST})
  install(TARGETS ${YGG_TARGET_PY}
  	  LIBRARY DESTINATION ${Python3_INSTALL_DIR})
else()
  install(TARGETS ${YGG_TARGET_PY}
          LIBRARY DESTINATION ${Python3_INSTALL_DIR}
	  RUNTIME DESTINATION ${Python3_INSTALL_DIR})
endif()

include(AddYggInterface)
add_library_dirs_to_rpath(
  ${YGG_TARGET_PY} DESTINATION ${YGG_CXX_LIBRARY_DEST}
)

if(YGG_INSPECT_TARGETS)
  include(CheckDLL)
  show_runtimes(${YGG_TARGET_PY})
endif()
