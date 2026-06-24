#!/bin/sh
set -euo pipefail

if [ -n "$ENABLE_CXX" ]; then
    ENABLE_CXX="ON"
fi
if [ -n "$ENABLE_Fortran" ]; then
    ENABLE_Fortran="ON"
fi
if [ -n "$ENABLE_Python" ]; then
    ENABLE_Python="OFF"
fi
if [ -n "$STANDALONE_Fortran" ]; then
    STANDALONE_Fortran="OFF"
fi

PYTHON=${PYTHON:-python}
Python_INCLUDE_DIR="$(${PYTHON} -c 'import sysconfig; print(sysconfig.get_path("include"))')"
Python_NumPy_INCLUDE_DIR="$(${PYTHON} -c 'import numpy; print(numpy.get_include())')"

CMAKE_ARGS+=" -DPython3_EXECUTABLE:PATH=${PYTHON}"
CMAKE_ARGS+=" -DPython3_INCLUDE_DIR:PATH=${Python_INCLUDE_DIR}"
CMAKE_ARGS+=" -DPython3_NumPy_INCLUDE_DIR=${Python_NumPy_INCLUDE_DIR}"

# Build C/CXX & Fortran libraries using cmake
if [ ! -d conda_build ]; then
    mkdir conda_build
fi

cmake -B conda_build -S ${SRC_DIR} \
      -G Ninja \
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON \
      -D CMAKE_MESSAGE_LOG_LEVEL:STRING=DEBUG \
      -D BUILD_CXX_LIBRARY:BOOL=$ENABLE_CXX \
      -D BUILD_Fortran_LIBRARY:BOOL=$ENABLE_Fortran \
      -D BUILD_Python_LIBRARY:BOOL=$ENABLE_Python \
      -D YGG_Fortran_STANDALONE:BOOL=$STANDALONE_Fortran \
      -D YGG_CXX_REQUIRED:BOOL=$ENABLE_CXX \
      -D YGG_Fortran_REQUIRED:BOOL=$ENABLE_Fortran \
      -D YGGINTERFACE_VERSION=${PKG_VERSION} \
      ${CMAKE_ARGS}
cmake --build conda_build -j${CPU_COUNT}
cmake --install conda_build --prefix ${PREFIX}
