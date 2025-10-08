#!/bin/sh
set -euo pipefail

# Build Python interface via pip
CMAKE_GENERATOR="Ninja" $PYTHON -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation

# Build C/CXX & Fortran libraries using cmake
if [ ! -d conda_build ]; then
    mkdir conda_build
fi
cd conda_build
cmake ${CMAKE_ARGS} \
      -G "Ninja" \
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON \
      -D Python3_EXECUTABLE=$PYTHON \
      -D BUILD_CXX_LIBRARY:BOOL=ON \
      -D BUILD_Python_LIBRARY:BOOL=OFF \
      -D BUILD_Fortran_LIBRARY:BOOL=OFF \
      -D YGG_CXX_REQUIRED:BOOL=ON \
      ..

cmake --install .

cd ..
