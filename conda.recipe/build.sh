#!/bin/sh
set -euo pipefail

PYTHON=${PYTHON:-python}
Python_INCLUDE_DIR="$(${PYTHON} -c 'import sysconfig; print(sysconfig.get_path("include"))')"
Python_NumPy_INCLUDE_DIR="$(${PYTHON} -c 'import numpy; print(numpy.get_include())')"

CMAKE_ARGS+=" -DPython3_EXECUTABLE:PATH=${PYTHON}"
CMAKE_ARGS+=" -DPython3_INCLUDE_DIR:PATH=${Python_INCLUDE_DIR}"
CMAKE_ARGS+=" -DPython3_NumPy_INCLUDE_DIR=${Python_NumPy_INCLUDE_DIR}"

# Build Python interface via pip
CMAKE_GENERATOR="Ninja" $PYTHON -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation

# Build C/CXX & Fortran libraries using cmake
if [ ! -d conda_build ]; then
    mkdir conda_build
fi

cmake -B conda_build -S ${SRC_DIR} \
      -G Ninja \
      -D VERBOSE:BOOL=ON \
      -D BUILD_CXX_LIBRARY:BOOL=ON \
      -D BUILD_Python_LIBRARY:BOOL=OFF \
      -D BUILD_Fortran_LIBRARY:BOOL=ON \
      -D YGG_CXX_REQUIRED:BOOL=ON \
      -D YGG_Fortran_REQUIRED:BOOL=ON \
      -D YGGINTERFACE_VERSION=${PKG_VERSION} \
      ${CMAKE_ARGS}
cmake --build conda_build -j${CPU_COUNT}
cmake --install conda_build
