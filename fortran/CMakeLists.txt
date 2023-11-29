set(YGG_TARGET_FORTRAN YggInterface_fortran)
if ((NOT FORCE_SPLIT_CXXFORTRAN) AND (NOT MSVC))
    enable_language(Fortran)
endif()

# Test case that can be removed
# include(SearchTools)
# list_search_directories(search_dirs)
# find_library_suffix(omv opencv_stereo SHARED REQUIRED)
# message(FATAL_ERROR "Found ${omv}")

include(YggAddFortranSubdirectory)
add_mixed_fortran_library(${YGG_TARGET_FORTRAN} SHARED
                          LANGUAGE CXX
                          c_wrappers.c YggInterface.f90)
add_library(YggInterface::fortran ALIAS ${YGG_TARGET_FORTRAN})

# Configure Fortran wrapper
set_target_properties(${YGG_TARGET_FORTRAN} PROPERTIES
                      VERSION ${PROJECT_VERSION}
                      SOVERSION ${PROJECT_VERSION_MAJOR}
                      OUTPUT_NAME ${YGG_TARGET_FORTRAN}
                      PUBLIC_HEADER YggInterface.f90)
target_include_directories(${YGG_TARGET_FORTRAN} PUBLIC
                           "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")
target_link_libraries(${YGG_TARGET_FORTRAN} PUBLIC YggInterface)
		      # YggInterface ${DEPS_LIB_NAMES} ${DEPS_LIBRARIES})
target_include_directories(${YGG_TARGET_FORTRAN} PUBLIC
                           ${DEPS_INCLUDE_DIRS})

message(STATUS "CMAKE_INSTALL_INCLUDEDIR = ${CMAKE_INSTALL_INCLUDEDIR}")
message(STATUS "CMAKE_CURRENT_BINARY_DIR = ${CMAKE_CURRENT_BINARY_DIR}")
# Install
if (YGGDRASIL_INSTALL)
   install(TARGETS ${YGG_TARGET_FORTRAN}
           EXPORT YggInterfaceTargets-fortran
           LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT YggInterface_Runtime NAMELINK_COMPONENT YggInterface_Development
           ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT YggInterface_Development
           PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT YggInterface_Development)
   export_components(COMPONENT fortran LIB_TYPE ${YGGDRASIL_LIB_TYPE})

   # Maybe it is handled automatically
   install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/YggInterface.mod
           DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
           COMPONENT YggInterface_Development)

   # configure_file(YggInterface.pc.in YggInterface.pc @ONLY)
   # install(FILES ${CMAKE_CURRENT_BINARY_DIR}/YggInterface.pc
   #         DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
   #         COMPONENT YggInterface_Development)
endif ()

include(CheckDLL)
show_runtimes(${YGG_TARGET_FORTRAN})

#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_FORTRAN_TESTS)
    add_definitions(-DYGG_TEST)
    add_subdirectory(tests)
    include(CTest)
endif()
