#!/bin/sh
set -euo pipefail

# Build Python interface via pip
CMAKE_GENERATOR="Ninja" $PYTHON -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation

# Build C/CXX & Fortran libraries using cmake
# if [ ! -d conda_build ]; then
#     mkdir conda_build
# fi
# cd conda_build
# cmake ${CMAKE_ARGS} \
#       -G "Ninja" \
#       -D Python3_EXECUTABLE=$PYTHON \
#       -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON \
#       -D BUILD_CPP_LIBRARY:BOOL=ON \
#       -D BUILD_PYTHON_LIBRARY:BOOL=ON \
#       -D YGG_Fortran_REQUIRED:BOOL=ON \
#       ..

# cmake --install .

# cd ..
