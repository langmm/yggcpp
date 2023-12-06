if(YGG_PYTHON_LIBRARY_WRAP)
    set(YGG_TARGET_PY _pyYggdrasil)
else()
    set(YGG_TARGET_PY pyYggdrasil)
endif()
message(STATUS "YGG_TARGET_PY = ${YGG_TARGET_PY}")
message(STATUS "Building Python bindings=Yes")
set(PYSOURCES
    ${CMAKE_SOURCE_DIR}/python/pyYggdrasil.cpp)
if (NOT YGG_LINK_PYTHON_TO_CPP)
    set(PYSOURCES ${PYSOURCES} ${SOURCES})
endif()
if (SKBUILD STREQUAL "2")
    Python3_add_library(${YGG_TARGET_PY} MODULE ${PYSOURCES})
else()
    add_library(${YGG_TARGET_PY} MODULE ${PYSOURCES})
endif()
target_include_directories(
          ${YGG_TARGET_PY} PUBLIC
          ${DEPS_INCLUDE_DIRS}
	  ${CMAKE_CURRENT_SOURCE_DIR}/../communication
	  ${RAPIDJSON_INCLUDE_DIRS})
if(YGG_LINK_PYTHON_TO_CPP)
    target_link_libraries(${YGG_TARGET_PY} PUBLIC ${YGG_TARGET_CPP})
endif()
target_link_libraries(
          ${YGG_TARGET_PY} PUBLIC
	  ${DEPS_LIB_NAMES_NO_PYTHON}
	  ${DEPS_LIBRARIES_NO_PYTHON})
target_compile_options(${YGG_TARGET_PY} PUBLIC
	  ${YGG_INSTALL_DEFS} -DRAPIDJSON_YGGDRASIL_PYTHON)
if(YGG_PYTHON_LIBRARY_WRAP)
    target_compile_options(${YGG_TARGET_PY} PRIVATE
              -DYGG_PYTHON_LIBRARY_WRAP)
endif()
if(SKBUILD)
    message(STATUS "SKBUILD = ${SKBUILD}")
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
    message(STATUS "YGG_CPP_LIBRARY_DEST = ${YGG_CPP_LIBRARY_DEST}")
    message(STATUS "Python_INSTALL_DIR = ${Python_INSTALL_DIR}")
    if(YGG_LINK_PYTHON_TO_CPP)
        if(WIN32)
            install(TARGETS ${YGG_TARGET_CPP}
                   LIBRARY DESTINATION ${YGG_CPP_LIBRARY_DEST}
		   PUBLIC_HEADER DESTINATION ${YGG_CPP_LIBRARY_DEST}
		   RUNTIME DESTINATION ${YGG_CPP_LIBRARY_DEST})
        else()
            install(TARGETS ${YGG_TARGET_CPP}
                    LIBRARY DESTINATION ${YGG_CPP_LIBRARY_DEST}
	            PUBLIC_HEADER DESTINATION ${YGG_CPP_LIBRARY_DEST})
        endif()
        install(TARGETS ${YGG_TARGET_PY}
  	        LIBRARY DESTINATION ${Python_INSTALL_DIR})
    else()
        if(WIN32)
            install(TARGETS ${YGG_TARGET_PY}
  	            LIBRARY DESTINATION ${Python_INSTALL_DIR}
		    RUNTIME DESTINATION ${Python_INSTALL_DIR})
	else()
            install(TARGETS ${YGG_TARGET_PY}
  	            LIBRARY DESTINATION ${Python_INSTALL_DIR})
	endif()
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
	    ${DEPS_LIB_NAMES_PYTHON}
	    ${DEPS_LIBRARIES_PYTHON})
endif(SKBUILD)
