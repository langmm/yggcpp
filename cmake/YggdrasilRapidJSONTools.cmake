# This file contains some useful macros for use by packages using
# yggdrasil-rapidjson as a dependency via find_package

macro(include_yggdrasil_rapidjson_macros)
  if(POLICY CMP0169)
    # Allows FetchContent_Populate to be called directly so that the
    # macros from a YggdrasilRapidJSON repository can be used without
    # including the subdirectory and initializing the YggdrasilRapidJSON
    # target with properties based on current option values
    cmake_policy(SET CMP0169 OLD)
  endif()
  if(NOT COMMAND yggdrasil_rapidjson_options)
    option(YGGDRASIL_RAPIDJSON_CLONE_IF_MISSING "Clone the YggdrasilRapidJSON library from GitHub if it cannot be located locally" OFF)
    set(YGGDRASIL_RAPIDJSON_REPO_DIR "" CACHE PATH "Existing directory containing the yggdrasil-rapidjson repository")
    set(YGGDRASIL_RAPIDJSON_REPO_BUILD_DIR "")
    set(YGGDRASIL_RAPIDJSON_MACROS_FILE "")

    if(YggdrasilRapidJSON_FOUND)
      message(FATAL_ERROR "YggdrasilRapidJSON_FOUND, but yggdrasil_rapidjson_options not defined")
    elseif(NOT YGGDRASIL_RAPIDJSON_REPO_DIR)
      if(WIN32)
        set(YGGDRASIL_RAPIDJSON_CONFIG_FILE_PATH_SUFFIXES "Library/cmake")
      else()
        set(YGGDRASIL_RAPIDJSON_CONFIG_FILE_PATH_SUFFIXES "lib/cmake/YggdrasilRapidJSON")
      endif()
      find_file(
        YGGDRASIL_RAPIDJSON_CONFIG_FILE
        YggdrasilRapidJSONConfig.cmake
        PATHS "${YggdrasilRapidJSON_DIR}"
        PATH_SUFFIXES "${YGGDRASIL_RAPIDJSON_CONFIG_FILE_PATH_SUFFIXES}"
      )
      unset(YGGDRASIL_RAPIDJSON_CONFIG_FILE_PATH_SUFFIXES)
      if(YGGDRASIL_RAPIDJSON_CONFIG_FILE)
        message(DEBUG "Located YggdrasilRapidJSON config file at ${YGGDRASIL_RAPIDJSON_CONFIG_FILE}")
        cmake_path(
          GET YGGDRASIL_RAPIDJSON_CONFIG_FILE
          PARENT_PATH YGGDRASIL_RAPIDJSON_MACROS_FILE
        )
        cmake_path(
          APPEND YGGDRASIL_RAPIDJSON_MACROS_FILE
          YggdrasilRapidJSONMacros.cmake
        )
      else()
        find_file(
          YGGDRASIL_RAPIDJSON_CONFIG_FILE
          YggdrasilRapidJSONConfig.cmake.in
          PATHS "${YggdrasilRapidJSON_DIR}"
        )
        if(YGGDRASIL_RAPIDJSON_CONFIG_FILE)
          message(DEBUG "Located YggdrasilRapidJSON config template file at ${YGGDRASIL_RAPIDJSON_CONFIG_FILE}")
          cmake_path(
            GET YGGDRASIL_RAPIDJSON_CONFIG_FILE
            PARENT_PATH YGGDRASIL_RAPIDJSON_REPO_DIR
          )
        endif()
      endif()
      unset(YGGDRASIL_RAPIDJSON_CONFIG_FILE)
    endif()
    if(YGGDRASIL_RAPIDJSON_REPO_DIR)
      if(NOT EXISTS "${YGGDRASIL_RAPIDJSON_REPO_DIR}")
        message(FATAL_ERROR "Directory specified by option YGGDRASIL_RAPIDJSON_REPO_DIR=\"${YGGDRASIL_RAPIDJSON_REPO_DIR}\" does not exist")
      endif()
      if(NOT IS_ABSOLUTE "${YGGDRASIL_RAPIDJSON_REPO_DIR}")
        cmake_path(
          ABSOLUTE_PATH YGGDRASIL_RAPIDJSON_REPO_DIR
          NORMALIZE
        )
      endif()
      include(FetchContent)
      FetchContent_Declare(
        YggdrasilRapidJSON
        SOURCE_DIR "${YGGDRASIL_RAPIDJSON_REPO_DIR}"
        EXCLUDE_FROM_ALL
        OVERRIDE_FIND_PACKAGE
      )
      FetchContent_Populate(YggdrasilRapidJSON)
      cmake_path(
        APPEND YGGDRASIL_RAPIDJSON_REPO_DIR "_build_for_${PROJECT_NAME}"
        OUTPUT_VARIABLE YGGDRASIL_RAPIDJSON_REPO_BUILD_DIR
      )
    elseif(NOT YGGDRASIL_RAPIDJSON_MACROS_FILE)
      if(YGGDRASIL_RAPIDJSON_CLONE_IF_MISSING)
        set(YGGDRASIL_RAPIDJSON_CLONE_MSG_LEVEL WARNING)
      else()
        set(YGGDRASIL_RAPIDJSON_CLONE_MSG_LEVEL FATAL_ERROR)
      endif()
      message(
        ${YGGDRASIL_RAPIDJSON_CLONE_MSG_LEVEL}
        "Could not locate the YggdrasilRapidJSON package via find_file (YggdrasilRapidJSON_DIR=${YggdrasilRapidJSON_DIR}), importing it from github as an external project..."
      )
      unset(YGGDRASIL_RAPIDJSON_CLONE_MSG_LEVEL)
      include(FetchContent)
      FetchContent_Declare(
        YggdrasilRapidJSON
        GIT_REPOSITORY https://github.com/cropsinsilico/yggdrasil-rapidjson.git
        GIT_TAG        origin/yggdrasil
        EXCLUDE_FROM_ALL
        OVERRIDE_FIND_PACKAGE
      )
      FetchContent_Populate(YggdrasilRapidJSON)
      set(YGGDRASIL_RAPIDJSON_REPO_DIR "${yggdrasilrapidjson_SOURCE_DIR}")
      set(YGGDRASIL_RAPIDJSON_REPO_BUILD_DIR "${yggdrasilrapidjson_BINARY_DIR}")
    endif()
    if(NOT YGGDRASIL_RAPIDJSON_MACROS_FILE)
      cmake_path(
        APPEND YGGDRASIL_RAPIDJSON_REPO_DIR
        YggdrasilRapidJSONMacros.cmake
        OUTPUT_VARIABLE YGGDRASIL_RAPIDJSON_MACROS_FILE
      )
    endif()
    message(STATUS "Including YggdrasilRapidJSON macros from \"${YGGDRASIL_RAPIDJSON_MACROS_FILE}\" (REPO_DIR=${YGGDRASIL_RAPIDJSON_REPO_DIR})")
    include("${YGGDRASIL_RAPIDJSON_MACROS_FILE}")
  endif()
endmacro()

macro(find_yggdrasil_rapidjson)
  include_yggdrasil_rapidjson_macros()
  if(YGGDRASIL_RAPIDJSON_REPO_DIR)
    # TODO: Move this into a generated dummy config file that can be
    # used by find_package
    foreach(suffix BUILD_EXAMPLES BUILD_TESTS BUILD_DOC)
      if(NOT YGGDRASIL_RAPIDJSON_${suffix})
        set(YGGDRASIL_RAPIDJSON_${suffix} OFF)
      endif()
    endforeach()
    add_subdirectory(
      "${YGGDRASIL_RAPIDJSON_REPO_DIR}"
      "${YGGDRASIL_RAPIDJSON_REPO_BUILD_DIR}"
      EXCLUDE_FROM_ALL
    )
    # Required for cmake <3.28 when EXCLUDE_FROM_ALL was added to
    # FetchContent_Declare
    set_property(
      DIRECTORY "${YGGDRASIL_RAPIDJSON_REPO_DIR}"
      PROPERTY EXCLUDE_FROM_ALL ON
    )
    set(YggdrasilRapidJSON_VERSION "${YGGDRASIL_RAPIDJSON_VERSION}")
    cmake_path(
      APPEND YGGDRASIL_RAPIDJSON_REPO_DIR "include"
      OUTPUT_VARIABLE YggdrasilRapidJSON_INCLUDE_DIRS
    )
    if(YggdrasilRapidJSON_VERSION VERSION_LESS "1.1.0.6")
      yggdrasil_rapidjson_options_config(LOCAL)
      yggdrasil_rapidjson_target_config(YggdrasilRapidJSON INTERFACE LOCAL)
    endif()
  else()
    find_package(YggdrasilRapidJSON REQUIRED)
  endif()

endmacro()
