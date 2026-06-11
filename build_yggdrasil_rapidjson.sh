#!/bin/bash
set -e

RJ_DIR=""
BUILD_DIR=""
INSTALL_DIR=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --rj-dir )
            RJ_DIR="$2"
	    shift
	    shift # past argument with value
	    ;;
        --build-dir )
            BUILD_DIR="$2"
	    shift
	    shift # past argument with value
	    ;;
        --install-dir )
            INSTALL_DIR="$2"
	    shift
	    shift # past argument with value
	    ;;
    esac
done
if [ ! -n "${RJ_DIR}" ]; then
    RJ_DIR="$(pwd)/yggdrasil-rapidjson"
fi
if [ ! -n "${BUILD_DIR}" ]; then
    BUILD_DIR="${RJ_DIR}/build"
fi
if [ ! -n "${INSTALL_DIR}" ]; then
    INSTALL_DIR="${RJ_DIR}/_install"
fi

if [ ! -d ${RJ_DIR} ]; then
    git clone --branch yggdrasil --recurse-submodules https://github.com/cropsinsilico/yggdrasil-rapidjson.git ${RJ_DIR}
fi
if [ ! -d ${BUILD_DIR} ]; then
    mkdir ${BUILD_DIR}
fi
if [ ! -d ${INSTALL_DIR} ]; then
    mkdir ${INSTALL_DIR}
fi

PYTHON=${PYTHON:-$(which python)}
Python_INCLUDE_DIR="$(${PYTHON} -c 'import sysconfig; print(sysconfig.get_path("include"))')"
Python_NumPy_INCLUDE_DIR="$(${PYTHON} -c 'import numpy; print(numpy.get_include())')"

CMAKE_ARGS+=" -DPython3_EXECUTABLE:PATH=${PYTHON}"
CMAKE_ARGS+=" -DPython3_INCLUDE_DIR:PATH=${Python_INCLUDE_DIR}"
CMAKE_ARGS+=" -DPython3_NumPy_INCLUDE_DIR=${Python_NumPy_INCLUDE_DIR}"
CMAKE_ARGS+=" -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}"

cmake -B ${BUILD_DIR} -S ${RJ_DIR} \
      -G Ninja \
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON \
      -D YGGDRASIL_RAPIDJSON_BUILD_EXAMPLES:BOOL=OFF \
      -D YGGDRASIL_RAPIDJSON_BUILD_TESTS:BOOL=OFF \
      -D YGGDRASIL_RAPIDJSON_BUILD_DOC:BOOL=OFF \
      ${CMAKE_ARGS}
cmake --build ${BUILD_DIR}
cmake --install ${BUILD_DIR} --prefix "${INSTALL_DIR}"
      


