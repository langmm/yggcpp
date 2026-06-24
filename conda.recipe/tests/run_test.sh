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

if [ ! -d build_speed ]; then
    mkdir build_speed
fi

cd build_speed

cmake -G Ninja \
      -D ENABLE_CXX:BOOL=$ENABLE_CXX \
      -D ENABLE_Fortran:BOOL=$ENABLE_Fortran \
      -D ENABLE_Python:BOOL=$ENABLE_Python \
      -D COMM:STRING=ALL \
      ../tests/speedtest 

cmake --build .
ctest

cd ..
