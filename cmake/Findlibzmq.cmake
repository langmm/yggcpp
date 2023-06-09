################################################################################
#  THIS FILE IS 100% GENERATED BY ZPROJECT; DO NOT EDIT EXCEPT EXPERIMENTALLY  #
#  Read the zproject/README.md for information about making permanent changes. #
################################################################################

if (NOT MSVC)
    find_package(PkgConfig)
    pkg_check_modules(PC_LIBZMQ "libzmq")
    if (PC_LIBZMQ_FOUND)
        # add CFLAGS from pkg-config file, e.g. draft api.
        add_definitions(${PC_LIBZMQ_CFLAGS} ${PC_LIBZMQ_CFLAGS_OTHER})
        # some libraries install the headers is a subdirectory of the include dir
        # returned by pkg-config, so use a wildcard match to improve chances of finding
        # headers and SOs.
        set(PC_LIBZMQ_INCLUDE_HINTS ${PC_LIBZMQ_INCLUDE_DIRS} ${PC_LIBZMQ_INCLUDE_DIRS}/*)
        set(PC_LIBZMQ_LIBRARY_HINTS ${PC_LIBZMQ_LIBRARY_DIRS} ${PC_LIBZMQ_LIBRARY_DIRS}/*)
    endif(PC_LIBZMQ_FOUND)
endif (NOT MSVC)

find_path (
        ${CMAKE_FIND_PACKAGE_NAME}_INCLUDE_DIRS
        NAMES zmq.h
        HINTS ${PC_LIBZMQ_INCLUDE_HINTS}
)

if (MSVC)
    # libzmq dll/lib built with MSVC is named using the Boost convention.
    # https://github.com/zeromq/czmq/issues/577
    # https://github.com/zeromq/czmq/issues/1972
    if (MSVC_IDE)
        set(MSVC_TOOLSET "-${CMAKE_VS_PLATFORM_TOOLSET}")
    else ()
        set(MSVC_TOOLSET "")
    endif ()

    # Retrieve ZeroMQ version number from zmq.h
    file(STRINGS "${${CMAKE_FIND_PACKAGE_NAME}_INCLUDE_DIRS}/zmq.h" zmq_version_defines
            REGEX "#define ZMQ_VERSION_(MAJOR|MINOR|PATCH)")
    foreach(ver ${zmq_version_defines})
        if(ver MATCHES "#define ZMQ_VERSION_(MAJOR|MINOR|PATCH) +([^ ]+)$")
            set(ZMQ_VERSION_${CMAKE_MATCH_1} "${CMAKE_MATCH_2}" CACHE INTERNAL "")
        endif()
    endforeach()

    set(_zmq_version ${ZMQ_VERSION_MAJOR}_${ZMQ_VERSION_MINOR}_${ZMQ_VERSION_PATCH})

    set(_zmq_debug_names
            "libzmq${MSVC_TOOLSET}-mt-gd-${_zmq_version}" # Debug, BUILD_SHARED
            "libzmq${MSVC_TOOLSET}-mt-sgd-${_zmq_version}" # Debug, BUILD_STATIC
            "libzmq-mt-gd-${_zmq_version}" # Debug, BUILD_SHARED
            "libzmq-mt-sgd-${_zmq_version}" # Debug, BUILD_STATIC
            )

    set(_zmq_release_names
            "libzmq${MSVC_TOOLSET}-mt-${_zmq_version}" # Release|RelWithDebInfo|MinSizeRel, BUILD_SHARED
            "libzmq${MSVC_TOOLSET}-mt-s-${_zmq_version}" # Release|RelWithDebInfo|MinSizeRel, BUILD_STATIC
            "libzmq-mt-${_zmq_version}" # Release|RelWithDebInfo|MinSizeRel, BUILD_SHARED
            "libzmq-mt-s-${_zmq_version}" # Release|RelWithDebInfo|MinSizeRel, BUILD_STATIC
            )

    find_library (${CMAKE_FIND_PACKAGE_NAME}_LIBRARY_DEBUG
            NAMES ${_zmq_debug_names}
            )

    find_library (${CMAKE_FIND_PACKAGE_NAME}_LIBRARY_RELEASE
            NAMES ${_zmq_release_names}
            )

    include(SelectLibraryConfigurations)
    select_library_configurations(${CMAKE_FIND_PACKAGE_NAME})
endif ()

if (NOT ${CMAKE_FIND_PACKAGE_NAME}_LIBRARIES)
    find_library (
            ${CMAKE_FIND_PACKAGE_NAME}_LIBRARIES
            NAMES libzmq zmq
            HINTS ${PC_LIBZMQ_LIBRARY_HINTS}
    )
endif ()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
        ${CMAKE_FIND_PACKAGE_NAME}
        REQUIRED_VARS ${CMAKE_FIND_PACKAGE_NAME}_LIBRARIES ${CMAKE_FIND_PACKAGE_NAME}_INCLUDE_DIRS
)
mark_as_advanced(
        ${CMAKE_FIND_PACKAGE_NAME}_FOUND
        ${CMAKE_FIND_PACKAGE_NAME}_LIBRARIES ${CMAKE_FIND_PACKAGE_NAME}_INCLUDE_DIRS
)

################################################################################
#  THIS FILE IS 100% GENERATED BY ZPROJECT; DO NOT EDIT EXCEPT EXPERIMENTALLY  #
#  Read the zproject/README.md for information about making permanent changes. #
################################################################################