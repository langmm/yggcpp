message(STATUS "Building Python bindings=Yes")
if(YGG_PYTHON_LIBRARY_WRAP)
    set(YGG_TARGET_PY _pyYggdrasil)
else()
    set(YGG_TARGET_PY pyYggdrasil)
endif()
set(SOURCES_PY
    ${CMAKE_SOURCE_DIR}/python/pyYggdrasil.cpp)
if (NOT YGG_LINK_PYTHON_TO_CPP)
    set(SOURCES_PY ${SOURCES_PY} ${SOURCES_CPP})
endif()
if (SKBUILD STREQUAL "2")
    Python3_add_library(${YGG_TARGET_PY} MODULE ${SOURCES_PY})
else()
    add_library(${YGG_TARGET_PY} MODULE ${SOURCES_PY})
endif()
target_include_directories(
          ${YGG_TARGET_PY} PUBLIC
          ${DEPS_INCLUDE_DIRS}
	  ${CMAKE_CURRENT_SOURCE_DIR}/../cpp
	  ${RAPIDJSON_INCLUDE_DIRS})
if(YGG_LINK_PYTHON_TO_CPP)
    target_link_libraries(${YGG_TARGET_PY} PUBLIC ${YGG_TARGET_CPP})
else()
    target_link_libraries(${YGG_TARGET_PY} PUBLIC
                          ${DEPS_LIBRARIES} ${DEPS_PUBLIC_CXX_LIBS})
    target_link_libraries(${YGG_TARGET_PY} PUBLIC
                          ${DEPS_LIBRARIES_PRIVATE})
endif()
target_compile_options(${YGG_TARGET_PY} PUBLIC
	  ${YGG_INSTALL_DEFS} -DRAPIDJSON_YGGDRASIL_PYTHON)
if(YGG_PYTHON_LIBRARY_WRAP)
    target_compile_options(${YGG_TARGET_PY} PRIVATE
              -DYGG_PYTHON_LIBRARY_WRAP)
endif()
if (ASAN_COMPILE_FLAGS)
    target_compile_options(
      ${YGG_TARGET_PY} PUBLIC ${ASAN_COMPILE_FLAGS})
endif()
if (ASAN_LINK_FLAGS)
    target_link_options(
      ${YGG_TARGET_PY} PUBLIC ${ASAN_LINK_FLAGS})
endif()
if(SKBUILD)
    if(SKBUILD STREQUAL "2")
        # set(YGG_CPP_LIBRARY_DEST pyYggdrasil/${CMAKE_INSTALL_LIBDIR})
	set(YGG_CPP_LIBRARY_DEST ".")
    else()
    	find_package(PythonExtensions REQUIRED)
	# Add C++ library as Python extension
	# Force python_extension_module to link to Python libraries
	# to allow dynamic lookup
	if(YGG_LINK_PYTHON_TO_CPP)
  	    set(CMAKE_CROSSCOMPILING ON)
	    python_extension_module(${YGG_TARGET_CPP})
	    set(CMAKE_CROSSCOMPILING OFF)
	endif()
	# Add Python extension
	python_extension_module(${YGG_TARGET_PY})
	set(YGG_CPP_LIBRARY_DEST ${CMAKE_INSTALL_LIBDIR})
    endif()
    if(NOT Python_INSTALL_DIR)
        if(SKBUILD STREQUAL "2")
	    # set(Python_INSTALL_DIR pyYggdrasil)
	    set(Python_INSTALL_DIR ".")
	else()
	    set(Python_INSTALL_DIR ".")
	endif()
    endif()
    if(YGG_LINK_PYTHON_TO_CPP)
        install(TARGETS ${YGG_TARGET_CPP}
                LIBRARY DESTINATION ${YGG_CPP_LIBRARY_DEST}
		PUBLIC_HEADER DESTINATION ${YGG_CPP_LIBRARY_DEST}
		RUNTIME DESTINATION ${YGG_CPP_LIBRARY_DEST})
        install(TARGETS ${YGG_TARGET_PY}
  	        LIBRARY DESTINATION ${Python_INSTALL_DIR})
    else()
        target_compile_definitions(
            ${YGG_TARGET_PY} PUBLIC ${YGG_INSTALL_DEFS})
	target_compile_options(
	    ${YGG_TARGET_PY} PUBLIC ${DEPS_PUBLIC_COMPILE_FLAGS}
	    ${DEPS_PUBLIC_CXX_COMPILE_FLAGS})
        install(TARGETS ${YGG_TARGET_PY}
  	        LIBRARY DESTINATION ${Python_INSTALL_DIR}
		RUNTIME DESTINATION ${Python_INSTALL_DIR})
    endif()
    if (NOT SKBUILD)
        install(DIRECTORY pyYggdrasil/ DESTINATION ${Python_INSTALL_DIR})
    endif()
    if(APPLE)
        set_target_properties(
	         ${YGG_TARGET_PY} PROPERTIES INSTALL_RPATH
		 "@loader_path/${YGG_CPP_LIBRARY_DEST}")
    else()
	set_target_properties(
	         ${YGG_TARGET_PY} PROPERTIES INSTALL_RPATH
	         "$ORIGIN/${YGG_CPP_LIBRARY_DEST}")
    endif()
else()
    target_link_libraries(
	    ${YGG_TARGET_PY} PRIVATE
	    ${DEPS_LIBRARIES_PYTHON})
endif(SKBUILD)
