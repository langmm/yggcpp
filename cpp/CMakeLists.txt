if(SKBUILD AND YGG_LINK_Python_TO_CXX)
    set(YGG_TARGET_CXX YggInterface_py)
else()
    set(YGG_TARGET_CXX YggInterface)
endif()
set(YGG_TARGET_CXX ${YGG_TARGET_CXX} PARENT_SCOPE)
if(SKBUILD AND YGG_LINK_Python_TO_CXX AND NOT YGGDRASIL_DISABLE_Python_C_API)
    add_library_python(${YGG_TARGET_CXX} SHARED ${SOURCES_CXX})
else()
    add_library(${YGG_TARGET_CXX} SHARED ${SOURCES_CXX})
    add_library(YggInterface::CXX ALIAS ${YGG_TARGET_CXX})
    add_library(YggInterface::C ALIAS ${YGG_TARGET_CXX})
    target_link_libraries(
	${YGG_TARGET_CXX} PUBLIC
	${DEPS_LIBRARIES_Python})
    set_target_properties(
        ${YGG_TARGET_CXX} PROPERTIES
        VERSION ${PROJECT_VERSION}
        SOVERSION ${PROJECT_VERSION_MAJOR}
        # DLL_NAME_WITH_SOVERSION ON
    )
    if (WIN32)
	set_target_properties(${YGG_TARGET_CXX}
	    PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
    endif()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        target_link_options(${YGG_TARGET_CXX}
	    PRIVATE /INCLUDE:yggdrasil_rapidjson_ARRAY_API)
    endif()
    list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_CXX})
    set(YGG_INSTALL_TARGETS ${YGG_INSTALL_TARGETS} PARENT_SCOPE)
endif()

if (MINGW)
    list(APPEND EXPORT_HEADER_ARGS EMPTY)
endif()
include(GenerateExportHeaderYggdrasil)
generate_export_header_yggdrasil(${YGG_TARGET_CXX} ${EXPORT_HEADER_ARGS})
set(YGG_INSTALL_INCLUDES ${YGG_INSTALL_INCLUDES} PARENT_SCOPE)

ygginterface_target_config(${YGG_TARGET_CXX} PUBLIC DEPS)
target_include_directories(
    ${YGG_TARGET_CXX} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
    $<INSTALL_INTERFACE:include/YggInterface>
)
target_compile_definitions(${YGG_TARGET_CXX} PUBLIC ${YGG_INSTALL_DEFS})
target_compile_features(${YGG_TARGET_CXX} PUBLIC cxx_std_14)

include(AddYggInterface)
add_library_dirs_to_rpath(${YGG_TARGET_CXX})

if(YGG_INSPECT_TARGETS)
  include(CheckDLL)
  show_runtimes(${YGG_TARGET_CXX} SHOW_ALL)
  # show_symbols(${YGG_TARGET_CXX})
endif()
