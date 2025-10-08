#!/bin/sh
set -euo pipefail

if [ ! -d build_speed ]; then
    mkdir build_speed
fi

cd build_speed

cmake -G "Ninja" ../tests/speedtest

cmake --build .

cd ..
