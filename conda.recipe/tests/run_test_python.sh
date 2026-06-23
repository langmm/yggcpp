#!/bin/sh
set -euo pipefail

if [ ! -d build_speed ]; then
    mkdir build_speed
fi

cd build_speed

cmake -G Ninja \
      -D ENABLE_C:BOOL=OFF \
      -D ENABLE_CXX:BOOL=OFF \
      -D ENABLE_Fortran:BOOL=OFF \
      -D ENABLE_Python:BOOL=ON \
      ../tests/speedtest 

cmake --build .
ctest

cd ..
