include(FortranCInterface)
FortranCInterface_VERIFY()
FortranCInterface_VERIFY(CXX)

add_library(YggInterface_fortran SHARED)
add_library(YggInterface::fortran ALIAS YggInterface_fortran)
set(CMAKE_Fortran_STANDARD 2003)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")

# Configure Fortran wrapper
target_sources(YggInterface_fortran
               PUBLIC YggInterface.f90
               PRIVATE c_wrappers.cpp)
set_target_properties(YggInterface_fortran PROPERTIES
                      VERSION ${PROJECT_VERSION}
                      SOVERSION ${PROJECT_VERSION_MAJOR}
                      EXPORT_NAME fortran
                      OUTPUT_NAME YggInterface_fortran
                      PUBLIC_HEADER YggInterface.f90)
set_target_properties(YggInterface_fortran PROPERTIES
                      PUBLIC_HEADER YggInterface.f90)
target_include_directories(YggInterface_fortran PUBLIC
                           "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")
target_link_libraries(YggInterface_fortran PUBLIC
		      YggInterface ${DEPS_LIB_NAMES} ${DEPS_LIBRARIES})
target_include_directories(YggInterface_fortran PUBLIC
                           ${DEPS_INCLUDE_DIRS})

message(STATUS "CMAKE_INSTALL_INCLUDEDIR = ${CMAKE_INSTALL_INCLUDEDIR}")
message(STATUS "CMAKE_CURRENT_BINARY_DIR = ${CMAKE_CURRENT_BINARY_DIR}")
# Install
if (YGGDRASIL_INSTALL)
   install(TARGETS YggInterface_fortran
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

#########
# Tests #
#########

if(YGG_BUILD_TESTS OR YGG_BUILD_FORTRAN_TESTS)
    add_definitions(-DYGG_TEST)
    add_subdirectory(tests)
    include(CTest)
endif()
