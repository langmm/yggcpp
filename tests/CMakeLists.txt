###############
# C++/C Tests #
###############

if(YGG_BUILD_TESTS OR YGG_BUILD_CXX_TESTS)
    set(SUBLIBFILE ${CMAKE_BINARY_DIR}/library_path.txt)
    add_custom_command(
        OUTPUT ${SUBLIBFILE}
        COMMAND echo "$<TARGET_FILE:${YGG_TARGET_CPP}>" > "${SUBLIBFILE}"
       DEPENDS ${YGG_TARGET_CPP}
       VERBATIM)
    add_custom_target(make_sublib ALL DEPENDS ${SUBLIBFILE})

    if(MSVC11)
        # required for VS2012 due to missing support for variadic templates
        add_definitions(-D_VARIADIC_MAX=10)
    endif(MSVC11)
    add_subdirectory(cpp)

endif()

#################
# Fortran Tests #
#################

if(YGG_BUILD_TESTS OR YGG_BUILD_FORTRAN_TESTS)
    add_subdirectory(fortran)
endif()

################
# Python Tests #
################

if(YGG_BUILD_TESTS OR YGG_BUILD_PYTHON_TESTS)
    add_subdirectory(python)
endif()
