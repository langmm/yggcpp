set(YGG_TARGET_FORTRAN YggInterface_fortran)
if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
    enable_language(Fortran)
endif()

include(YggAddFortranSubdirectory)
add_mixed_fortran_library(${YGG_TARGET_FORTRAN} SHARED
                          LANGUAGE CXX
                          c_wrappers.c YggInterface.f90)
add_library(YggInterface::Fortran ALIAS ${YGG_TARGET_FORTRAN})

# Configure Fortran wrapper
set_target_properties(${YGG_TARGET_FORTRAN} PROPERTIES
                      VERSION ${PROJECT_VERSION}
                      SOVERSION ${PROJECT_VERSION_MAJOR}
                      OUTPUT_NAME ${YGG_TARGET_FORTRAN})
target_include_directories(${YGG_TARGET_FORTRAN} PUBLIC
                           "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")
target_link_libraries(${YGG_TARGET_FORTRAN} PUBLIC YggInterface)
target_include_directories(${YGG_TARGET_FORTRAN} PRIVATE
                           ${DEPS_INCLUDE_DIRS})
list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_FORTRAN})
set(YGG_INSTALL_TARGETS ${YGG_INSTALL_TARGETS} PARENT_SCOPE)

# Install
list(APPEND YGG_INSTALL_INCLUDES
     ${CMAKE_CURRENT_BINARY_DIR}/YggInterface.mod)

include(CheckDLL)
show_runtimes(${YGG_TARGET_FORTRAN})

#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_FORTRAN_TESTS)
    add_subdirectory(tests)
    include(CTest)
endif()
