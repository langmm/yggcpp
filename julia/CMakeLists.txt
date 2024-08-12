cmake_minimum_required(VERSION 3.5)
set(CMAKE_MACOSX_RPATH 1)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
include(BuildTools)

set(YGG_TARGET_Julia YggInterface_julia)
set(SOURCES_Julia YggInterface_julia.cpp)

python_code_generation(
  generate_generic.runner IS_MODULE
  ARGUMENTS --language=julia
  OUTPUTS julia/YggInterface_julia.cpp julia/YggInterface.jl
  WORKING_DIRECTORY ../utils
)

get_target_property(JlCxx_location JlCxx::cxxwrap_julia LOCATION)
get_filename_component(JlCxx_location ${JlCxx_location} DIRECTORY)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib;${JlCxx_location}")

message(STATUS "Found JlCxx at ${JlCxx_location}")

add_library(${YGG_TARGET_Julia} SHARED ${SOURCES_Julia})
add_library(YggInterface::Julia ALIAS ${YGG_TARGET_Julia})

target_link_libraries(
  ${YGG_TARGET_Julia} PRIVATE
  YggInterface JlCxx::cxxwrap_julia
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

# install(TARGETS
#   ${YGG_TARGET_Julia}
#   LIBRARY DESTINATION lib
#   ARCHIVE DESTINATION lib
#   RUNTIME DESTINATION lib)


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

include(CheckDLL)
show_runtimes(${YGG_TARGET_Julia})
show_symbols(${YGG_TARGET_Julia})
