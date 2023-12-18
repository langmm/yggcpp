set -e

LANGUAGE=""
REBUILD=""
DONT_BUILD=""
DO_C=""
DO_CXX=""
DO_FORTRAN=""
DO_PYTHON=""
WITH_ASAN=""
NO_CORE=""
CMAKE_FLAGS="-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON -DYGG_SKIP_VALGRIND_TESTS=ON"
CMAKE_FLAGS_LIB=""
CMAKE_FLAGS_SPEED=""
CMAKE_PREFIX_PATH=""
WITH_LLDB=""
DO_SYMBOLS=""
DONT_TEST=""
TEST_TYPE="unit"
TEST_FLAGS=""
NO_DEBUG_MSG=""
CONFIG_FLAGS=""
N_MSG="100"
S_MSG="100"
COMM="DEFAULT_COMM"
INSTALL_DIR="$(pwd)/_install"

while [[ $# -gt 0 ]]; do
    case $1 in
	-c )
	    DO_C="TRUE"
	    shift # past argument with no value
	    ;;
	--cxx )
	    DO_CXX="TRUE"
	    shift # past argument with no value
	    ;;
	-p | --python )
	    DO_PYTHON="TRUE"
	    shift # past argument with no value
	    ;;
	-f | --fortran )
	    DO_FORTRAN="TRUE"
	    shift # past argument with no value
	    ;;
	-l | --language )
	    LANGUAGE="$2"
	    shift
	    shift
	    ;;
	--rebuild )
	    REBUILD="TRUE"
	    shift
	    ;;
	--dont-build )
	    DONT_BUILD="TRUE"
	    shift # past argument with no value
	    ;;
	--with-asan )
	    WITH_ASAN="TRUE"
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_BUILD_ASAN=ON -DYGG_BUILD_UBSAN=ON"
	    shift # past argument with no value
	    ;;
	--without-core )
	    NO_CORE="TRUE"
	    shift # past argument with no value
	    ;;
	--using-ipc )
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DUSING_IPC=1"
	    shift # past argument with no value
	    ;;
	--with-lldb )
	    WITH_LLDB="TRUE"
	    shift # past argument with no value 
	    ;;
	--split-cfort )
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DFORCE_SPLIT_CXXFORTRAN=1"
	    shift # past argument with no value
	    ;;
	--symbols )
	    DO_SYMBOLS="TRUE"
	    DONT_TEST="TRUE"
	    DONT_BUILD="TRUE"
	    shift # past argument with no value
	    ;;
	--dont-test )
	    DONT_TEST="TRUE"
	    shift # past argument with no value
	    ;;
	--verbose )
	    TEST_FLAGS="${TEST_FLAGS} --output-on-failure -VV"
	    shift # past argument with no value
	    ;;
	--rj-wrapper )
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DWRAP_RAPIDJSON_FOR_DLL=1"
	    shift # past argument with no value
	    ;;
	--python-link-cpp )
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_LINK_PYTHON_TO_CPP=1"
	    shift # past argument with no value
	    ;;
	--disable-python )
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGGDRASIL_DISABLE_PYTHON_C_API=1"
	    shift # past argument with no value
	    ;;
	--no-debug )
	    NO_DEBUG_MSG="TRUE"
	    shift # past argument with no value
	    ;;
	--local-rj )
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DRAPIDJSON_INCLUDE_DIRS=/Users/langmm/rapidjson/include"
	    shift # past argument with no value
	    ;;
	--config )
	    CONFIG_FLAGS="--config $2"
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DCMAKE_BUILD_TYPE=$2"
	    TEST_FLAGS="${TEST_FLAGS} -C $2"
	    shift
	    shift
	    ;;
	--install-dir )
	    INSTALL_DIR="$2"
	    shift
	    shift
	    ;;
	-DCMAKE_INSTALL_PREFIX=* )
	    INSTALL_DIR="${1#-DCMAKE_INSTALL_PREFIX=}"
	    shift
	    ;;
	--speed )
	    TEST_TYPE="speed"
	    shift # past argument with no value
	    ;;
	-n | --n-msg )
	    N_MSG="$2"
	    shift
	    shift
	    ;;
	-s | --s-msg )
	    S_MSG="$2"
	    shift
	    shift
	    ;;
	--comm )
	    COMM="$2"
	    shift
	    shift
	    ;;
	-DCMAKE_PREFIX_PATH=* )
	    new_path=${1#"-DCMAKE_PREFIX_PATH="}
	    if [ -n "$CMAKE_PREFIX_PATH" ]; then
		CMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH};${new_path}"
	    else
		CMAKE_PREFIX_PATH="${new_path}"
	    fi
	    shift
	    ;;
	-* | --* )
	    CMAKE_FLAGS="${CMAKE_FLAGS} $1"
	    shift
	    ;;
	*)
	    shift
	    ;;
    esac
done

if [ -n "$INSTALL_DIR" ]; then
    echo "INSTALL_DIR = ${INSTALL_DIR}"
    CMAKE_FLAGS="${CMAKE_FLAGS} -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}"
fi

if [[ "$CMAKE_FLAGS" == *"-DYGG_BUILD_ASAN=ON"* ]]; then
    WITH_ASAN="TRUE"
fi

# if [[ "$TEST_TYPE" == "speed" ]]; then
#     NO_DEBUG_MSG="TRUE"
# fi

if [ "$LANGUAGE" = "C" ]; then
    DO_C="TRUE"
elif [ "$LANGUAGE" = "CXX" ]; then
    DO_CXX="TRUE"
elif [ "$LANGUAGE" = "FORTRAN" ]; then
    DO_FORTRAN="TRUE"
elif [ "$LANGUAGE" = "PYTHON" ]; then
    DO_PYTHON="TRUE"
fi

if [ ! -n "$DO_C" ] && [ ! -n "$DO_CXX" ] && [ ! -n "$DO_FORTRAN" ] && [ ! -n "$DO_PYTHON" ]; then
    DO_C="TRUE"
    DO_CXX="TRUE"
    DO_FORTRAN="TRUE"
    DO_PYTHON="TRUE"
    if [[ "$TEST_TYPE" == "unit" ]]; then
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_BUILD_TESTS=ON"
    fi
fi

if [ -n "$DO_C" ] || [ -n "$DO_CXX" ] || [ -n "$DO_FORTRAN" ] || [ -n "$DO_PYTHON" ]; then
    if [ -n "$DO_C" ]; then
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_C=ON"
    else
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_C=OFF"
    fi
    if [ -n "$DO_CXX" ]; then
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_CXX=ON"
    else
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_CXX=OFF"
    fi
    if [ -n "$DO_C" ] || [ -n "$DO_CXX" ]; then
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_CPP_LIBRARY=ON"
	if [[ "$TEST_TYPE" == "unit" ]]; then
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_BUILD_CXX_TESTS=ON"
	fi
    else
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_CPP_LIBRARY=OFF"
    fi
    if [ -n "$DO_FORTRAN" ]; then
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_FORTRAN_LIBRARY=ON -DYGG_Fortran_REQUIRED=ON -DYGG_ENABLE_ELF=OFF"
	if [[ "$TEST_TYPE" == "unit" ]]; then
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_BUILD_FORTRAN_TESTS=ON"
	fi
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_Fortran=ON"
    else
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_FORTRAN_LIBRARY=OFF"
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_Fortran=OFF"
    fi
    if [ -n "$DO_PYTHON" ]; then
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_PYTHON_LIBRARY=ON"
	if [[ "$TEST_TYPE" == "unit" ]]; then
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_BUILD_PYTHON_TESTS=ON"
	fi
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_Python=ON"
    else
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_Python=OFF"
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_PYTHON_LIBRARY=OFF"
    fi
fi

if [ ! -n "$NO_DEBUG_MSG" ]; then
    CMAKE_FLAGS="${CMAKE_FLAGS} -DYGG_DEBUG_LEVEL=5"
fi

if [ -n "$CMAKE_PREFIX_PATH" ]; then
    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}"
fi

echo "CMAKE_FLAGS = ${CMAKE_FLAGS}"
echo "CMAKE_FLAGS_LIB = ${CMAKE_FLAGS_LIB}"
echo "CMAKE_FLAGS_SPEED = ${CMAKE_FLAGS_SPEED}"

# if [ -n "$WITH_ASAN" ]; then
#     export ASAN_OPTIONS=symbolize=1
#     export ASAN_SYMBOLIZER_PATH=$(which llvm-symbolizer)
# fi

if [ -n "$REBUILD" ]; then
    DONT_BUILD=""
    if [ -d "build" ]; then
	rm -rf "build"
    fi
    if [ -d "_skbuild" ]; then
	rm -rf "_skbuild"
    fi
    if [ -d "$INSTALL_DIR" ]; then
	rm -rf "$INSTALL_DIR"
    fi
    if [ -f "src/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so" ]; then
	rm "src/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so"
    fi
    if [ -f "src/pyYggdrasil/lib/libYggInterface_py.dylib" ]; then
	rm "src/pyYggdrasil/lib/libYggInterface_py.dylib"
    fi
fi
# export CMAKE_ARGS=${CMAKE_FLAGS}
if [ -n "$DO_PYTHON" ] && [ -n "$NO_CORE" ]; then
    export CMAKE_ARGS="${CMAKE_FLAGS} ${CMAKE_FLAGS_LIB}"
    if [ ! -n "$DONT_BUILD" ]; then
	python3 setup.py build_ext --inplace
    fi
    if [ -n "$WITH_ASAN" ] && [ ! -n "$DYLD_INSERT_LIBRARIES" ]; then
	export DYLD_INSERT_LIBRARIES=$(clang -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
    fi
    cd python/pyYggdrasil
    export TEST_DIR=../test
    if [[ "$TEST_TYPE" == "unit" ]] && [ ! -n "$DONT_TEST" ]; then
	export PYTHONFAULTHANDLER=1
	if [ -n "$WITH_LLDB" ]; then
	    lldb -o 'run' -o 'quit' -- $(which python3) -m pytest -svx $TEST_DIR
	else
	    # python3 -m pytest -svx $TEST_DIR
	    pytest -svx $TEST_DIR
	fi
    fi
    cd ../../
else
    # DYLD_PRINT_LIBRARIES=1
    # DYLD_PRINT_APIS=1
    # DYLD_PRINT_WARNINGS=1
    # LDFLAGS='-undefined error'
    if [ ! -d "build" ]; then
	mkdir build
    fi
    cd build
    if [ ! -n "$DONT_BUILD" ]; then
	cmake .. $CMAKE_FLAGS $CMAKE_FLAGS_LIB
	cmake --build . $CONFIG_FLAGS
	# Need install here to ensure that cmake config files are in place
	cmake --install . --prefix "$INSTALL_DIR" $CONFIG_FLAGS
    fi
    if [[ "$TEST_TYPE" == "unit" ]] && [ ! -n "$DONT_TEST" ]; then
	if [ -n "$WITH_ASAN" ] && [ ! -n "$DYLD_INSERT_LIBRARIES" ]; then
	    export DYLD_INSERT_LIBRARIES=$(clang -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
	fi
	if [ -n "$WITH_LLDB" ]; then
	    if [ -n "$DO_FORTRAN" ]; then
		lldb -o 'run' -o 'quit' fortran/tests/fortran_testsuite -- test_ygg_input_1_
	    else
		lldb -o 'run' -o 'quit' test/unittest
	    fi
	else
	    ctest $TEST_FLAGS --stop-on-failure
	    # make test ARGS="--stop-on-failure"
	fi
    fi
    cd ..
fi

echo "INSTALL_DIR = \"${INSTALL_DIR}\""
if [ -n "$CMAKE_PREFIX_PATH" ]; then
    CMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH};${INSTALL_DIR}"
else
    CMAKE_PREFIX_PATH="${INSTALL_DIR}"
fi
CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}"

if [[ "$TEST_TYPE" == "speed" ]]; then
    if [ ! -n "$DONT_BUILD" ]; then
	if [ -d "build_speed" ]; then
	    rm -rf build_speed
	fi
	if [ ! -d "build_speed" ]; then
	    mkdir build_speed
	fi
    fi
    cd build_speed
    if [ -n "$WITH_ASAN" ] && [ ! -n "$DYLD_INSERT_LIBRARIES" ]; then
	export DYLD_INSERT_LIBRARIES=$(clang -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
    fi
    if [ ! -n "$DONT_BUILD" ]; then
	cmake ../test/speedtest "-DYggInterface_DIR=$INSTALL_DIR/lib/cmake/YggInterface" -DN_MSG=$N_MSG -DS_MSG=$S_MSG -DCOMM=$COMM $CMAKE_FLAGS $CMAKE_FLAGS_SPEED
	cmake --build .
    fi
    if [ ! -n "$DONT_TEST" ]; then
	echo "DYLD_INSERT_LIBRARIES = ${DYLD_INSERT_LIBRARIES}"
	ctest $TEST_FLAGS
	# make test
    fi
    cd ..
fi

if [ -n "$DO_SYMBOLS" ]; then
    if [[ "$TEST_TYPE" == "speed" ]]; then
	SYM_LANG="C"
	python utils/check_symbols.py build_speed/${SYM_LANG}_build/speedtest_${SYM_LANG} build/libYggInterface.dylib  &> symbols.txt
    else
	python utils/check_symbols.py build/test/unittest build/libYggInterface.dylib  &> symbols.txt
    fi
fi
