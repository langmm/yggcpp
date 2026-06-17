#!/bin/sh
set -euo pipefail

if [ ! -d build_speed ]; then
    mkdir build_speed
fi

cd build_speed

cmake -G Ninja \
      -D ENABLE_Python:BOOL=OFF \
      ../tests/speedtest 

cmake --build .
ctest

cd ..
