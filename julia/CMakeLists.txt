cmake_minimum_required(VERSION 3.5)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
include(BuildTools)
include(AddYggInterface)

set(YGG_TARGET_Julia YggInterface_julia)
set(SOURCES_Julia ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface_julia.cpp)

set(GENERATION_ARGS)
if(WRAP_RAPIDJSON_FOR_DLL)
  list(APPEND GENERATION_ARGS --wrap-rapidjson)
endif()
if(RAPIDJSON_INCLUDE_DIRS)
  list(APPEND GENERATION_ARGS --rapidjson-include-dirs=${RAPIDJSON_INCLUDE_DIRS})
endif()

python_code_generation(
  wrap_julia ../utils/generate_generic
  ENTRY_POINT -m generate_generic.runner
  ARGUMENTS --language=julia ${GENERATION_ARGS}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/utils
  SOURCES
  ${CMAKE_SOURCE_DIR}/cpp/include/utils/enums.hpp
  ${CMAKE_SOURCE_DIR}/cpp/include/communicators/CommBase.hpp
  ${CMAKE_SOURCE_DIR}/cpp/include/communicators/WrapComm.hpp
  BYPRODUCTS
  ${SOURCES_Julia}
  ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface/src/YggInterface.jl
)

add_yggdrasil_cmake_config(
  YggInterface_Julia_PACKAGE_DIR
  TYPE PATH
  DESCRIPTION "Directory containing the Julia YggInterface package"
  VALUE ${CMAKE_CURRENT_SOURCE_DIR}/YggInterface
)

get_target_property(JlCxx_location JlCxx::cxxwrap_julia LOCATION)
get_filename_component(JlCxx_location ${JlCxx_location} DIRECTORY)
message(STATUS "Found JlCxx at ${JlCxx_location}")

add_library(${YGG_TARGET_Julia} SHARED ${SOURCES_Julia})
add_library(YggInterface::Julia ALIAS ${YGG_TARGET_Julia})

target_link_libraries(
  ${YGG_TARGET_Julia} PRIVATE
  YggInterface JlCxx::cxxwrap_julia JlCxx::cxxwrap_julia_stl
)
target_include_directories(
  ${YGG_TARGET_Julia} PUBLIC
  "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)
target_include_directories(
  ${YGG_TARGET_Julia} PRIVATE
  ${DEPS_INCLUDE_DIRS}
)

list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_Julia})
set(YGG_INSTALL_TARGETS ${YGG_INSTALL_TARGETS} PARENT_SCOPE)
set(YGG_INSTALL_CONFIG ${YGG_INSTALL_CONFIG} PARENT_SCOPE)
set(YGG_TARGET_Julia ${YGG_TARGET_Julia} PARENT_SCOPE)

target_compile_definitions(
  ${YGG_TARGET_Julia} PRIVATE -DJULIA_ENABLE_THREADING=1
)

if (COVERAGE_C_COMPILE_FLAGS)
  target_compile_options(
    ${YGG_TARGET_Julia} PUBLIC
    "$<$<COMPILE_LANGUAGE:C>:${COVERAGE_C_COMPILE_FLAGS}>"
    "$<$<COMPILE_LANGUAGE:CXX>:${COVERAGE_C_COMPILE_FLAGS}>")
endif()
if (COVERAGE_C_LINK_FLAGS)
  target_link_options(
    ${YGG_TARGET_Julia} PUBLIC
    "$<$<LINK_LANGUAGE:C>:${COVERAGE_C_LINK_FLAGS}>"
    "$<$<LINK_LANGUAGE:CXX>:${COVERAGE_C_LINK_FLAGS}>")
endif()
if (COVERAGE_C_LIBRARIES)
  target_link_libraries(
    ${YGG_TARGET_Julia} PRIVATE ${COVERAGE_C_LIBRARIES})
endif()

include(AddYggInterface)
add_library_dirs_to_rpath(
  ${YGG_TARGET_Julia}
  ADDITIONAL_DIRECTORIES ${CMAKE_INSTALL_PREFIX}/lib
  ${JlCxx_location} ${Julia_LIBRARY_DIR}
)

include(CheckDLL)
show_runtimes(${YGG_TARGET_Julia})
show_symbols(${YGG_TARGET_Julia})
