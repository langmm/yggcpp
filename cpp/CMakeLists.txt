if(SKBUILD AND YGG_LINK_Python_TO_CXX)
    set(YGG_TARGET_CXX YggInterface_py)
else()
    set(YGG_TARGET_CXX YggInterface)
endif()
set(YGG_TARGET_CXX ${YGG_TARGET_CXX} PARENT_SCOPE)
if(SKBUILD AND YGG_LINK_Python_TO_CXX AND NOT YGGDRASIL_DISABLE_Python_C_API)
    add_library_python(${YGG_TARGET_CXX} SHARED ${SOURCES_CXX})
    target_compile_options(
        ${YGG_TARGET_CXX} PUBLIC
 	-DRAPIDJSON_YGGDRASIL_Python -DYGG_LINK_Python_TO_CXX)
else()
    add_library(${YGG_TARGET_CXX} SHARED ${SOURCES_CXX})
    add_library(YggInterface::CXX ALIAS ${YGG_TARGET_CXX})
    add_library(YggInterface::C ALIAS ${YGG_TARGET_CXX})
    target_link_libraries(
	${YGG_TARGET_CXX} PUBLIC
	${DEPS_LIBRARIES_Python})
    if (WIN32)
	set_target_properties(${YGG_TARGET_CXX}
	    PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
    endif()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        target_link_options(${YGG_TARGET_CXX}
	    PRIVATE /INCLUDE:rapidjson_ARRAY_API)
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

target_link_libraries(
    ${YGG_TARGET_CXX} PRIVATE ${DEPS_LIBRARIES_PRIVATE})
target_link_libraries(
    ${YGG_TARGET_CXX} PUBLIC ${DEPS_LIBRARIES}
    "$<$<LINK_LANGUAGE:C>:${DEPS_PUBLIC_C_LIBS}>"
    "$<$<LINK_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_LIBS}>"
    "$<$<LINK_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_LIBS}>")
target_include_directories(
    ${YGG_TARGET_CXX} PUBLIC ${DEPS_INCLUDE_DIRS}
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${RAPIDJSON_INCLUDE_DIRS}>
    $<INSTALL_INTERFACE:include/YggInterface>
    $<INSTALL_INTERFACE:include/YggInterface/rapidjson/include>)
target_compile_definitions(
    ${YGG_TARGET_CXX} PUBLIC ${YGG_INSTALL_DEFS})
target_compile_options(
    ${YGG_TARGET_CXX} PUBLIC ${DEPS_PUBLIC_COMPILE_FLAGS}
    "$<$<COMPILE_LANGUAGE:C>:${DEPS_PUBLIC_C_COMPILE_FLAGS}>"
    "$<$<COMPILE_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_COMPILE_FLAGS}>"
    "$<$<COMPILE_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_COMPILE_FLAGS}>")
target_link_options(
    ${YGG_TARGET_CXX} PUBLIC ${DEPS_PUBLIC_LINK_FLAGS}
    "$<$<LINK_LANGUAGE:C>:${DEPS_PUBLIC_C_LINK_FLAGS}>"
    "$<$<LINK_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_LINK_FLAGS}>"
    "$<$<LINK_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_LINK_FLAGS}>")
target_compile_features(
    ${YGG_TARGET_CXX} PUBLIC cxx_std_14)

if (COVERAGE_C_COMPILE_FLAGS)
    target_compile_options(
        ${YGG_TARGET_CXX} PUBLIC
	"$<$<COMPILE_LANGUAGE:C>:${COVERAGE_C_COMPILE_FLAGS}>"
	"$<$<COMPILE_LANGUAGE:CXX>:${COVERAGE_C_COMPILE_FLAGS}>")
endif()
if (COVERAGE_C_LINK_FLAGS)
    target_link_options(
        ${YGG_TARGET_CXX} PUBLIC
	"$<$<LINK_LANGUAGE:C>:${COVERAGE_C_LINK_FLAGS}>"
	"$<$<LINK_LANGUAGE:CXX>:${COVERAGE_C_LINK_FLAGS}>")
endif()
if (COVERAGE_C_LIBRARIES)
    target_link_libraries(
        ${YGG_TARGET_CXX} PRIVATE ${COVERAGE_C_LIBRARIES})
endif()

include(CheckDLL)
show_runtimes(${YGG_TARGET_CXX})
# show_symbols(${YGG_TARGET_CXX})
