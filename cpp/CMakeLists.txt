if(SKBUILD AND YGG_LINK_PYTHON_TO_CPP)
    set(YGG_TARGET_CPP YggInterface_py)
else()
    set(YGG_TARGET_CPP YggInterface)
endif()
set(YGG_TARGET_CPP ${YGG_TARGET_CPP} PARENT_SCOPE)
if(SKBUILD AND YGG_LINK_PYTHON_TO_CPP AND NOT YGGDRASIL_DISABLE_PYTHON_C_API)
    add_library_python(${YGG_TARGET_CPP} SHARED ${SOURCES_CPP})
    target_compile_options(
        ${YGG_TARGET_CPP} PUBLIC
 	-DRAPIDJSON_YGGDRASIL_PYTHON -DYGG_LINK_PYTHON_TO_CPP)
else()
    add_library(${YGG_TARGET_CPP} SHARED ${SOURCES_CPP})
    add_library(YggInterface::CXX ALIAS ${YGG_TARGET_CPP})
    add_library(YggInterface::C ALIAS ${YGG_TARGET_CPP})
    target_link_libraries(
	${YGG_TARGET_CPP} PUBLIC
	${DEPS_LIBRARIES_PYTHON})
    if (WIN32)
	set_target_properties(${YGG_TARGET_CPP}
	    PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
    endif()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        target_link_options(${YGG_TARGET_CPP}
	    PRIVATE /INCLUDE:rapidjson_ARRAY_API)
    endif()
    list(APPEND YGG_INSTALL_TARGETS ${YGG_TARGET_CPP})
    set(YGG_INSTALL_TARGETS ${YGG_INSTALL_TARGETS} PARENT_SCOPE)
endif()
target_link_libraries(
    ${YGG_TARGET_CPP} PRIVATE ${DEPS_LIBRARIES_PRIVATE})
target_link_libraries(
    ${YGG_TARGET_CPP} PUBLIC ${DEPS_LIBRARIES}
    "$<$<LINK_LANGUAGE:C>:${DEPS_PUBLIC_C_LIBS}>"
    "$<$<LINK_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_LIBS}>"
    "$<$<LINK_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_LIBS}>")
target_include_directories(
    ${YGG_TARGET_CPP} PUBLIC ${DEPS_INCLUDE_DIRS}
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${RAPIDJSON_INCLUDE_DIRS}>
    $<INSTALL_INTERFACE:include/YggInterface>
    $<INSTALL_INTERFACE:include/YggInterface/rapidjson/include>)
target_compile_definitions(
    ${YGG_TARGET_CPP} PUBLIC ${YGG_INSTALL_DEFS})
target_compile_options(
    ${YGG_TARGET_CPP} PUBLIC ${DEPS_PUBLIC_COMPILE_FLAGS}
    "$<$<COMPILE_LANGUAGE:C>:${DEPS_PUBLIC_C_COMPILE_FLAGS}>"
    "$<$<COMPILE_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_COMPILE_FLAGS}>"
    "$<$<COMPILE_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_COMPILE_FLAGS}>")
target_link_options(
    ${YGG_TARGET_CPP} PUBLIC ${DEPS_PUBLIC_LINK_FLAGS}
    "$<$<LINK_LANGUAGE:C>:${DEPS_PUBLIC_C_LINK_FLAGS}>"
    "$<$<LINK_LANGUAGE:CXX>:${DEPS_PUBLIC_CXX_LINK_FLAGS}>"
    "$<$<LINK_LANGUAGE:Fortran>:${DEPS_PUBLIC_Fortran_LINK_FLAGS}>")
target_compile_features(
    ${YGG_TARGET_CPP} PUBLIC cxx_std_14)
    
include(CheckDLL)
show_runtimes(${YGG_TARGET_CPP})
