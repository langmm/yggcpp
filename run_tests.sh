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
WITH_LLDB=""
DO_SYMBOLS=""
DONT_TEST=""
TEST_FLAGS=""
NO_DEBUG_MSG=""
CONFIG_FLAGS=""
SPEED_TEST=""
N_MSG="100"
S_MSG="100"
COMM="DEFAULT_COMM"

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
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DYGG_BUILD_ASAN=ON -DYGG_BUILD_UBSAN=ON"
	    shift # past argument with no value
	    ;;
	--without-core )
	    NO_CORE="TRUE"
	    shift # past argument with no value
	    ;;
	--using-ipc )
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DUSING_IPC=1"
	    shift # past argument with no value
	    ;;
	--with-lldb )
	    WITH_LLDB="TRUE"
	    shift # past argument with no value 
	    ;;
	--split-cfort )
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DFORCE_SPLIT_CXXFORTRAN=1"
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
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DWRAP_RAPIDJSON_FOR_DLL=1"
	    shift # past argument with no value
	    ;;
	--python-link-cpp )
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DYGG_LINK_PYTHON_TO_CPP=1"
	    shift # past argument with no value
	    ;;
	--disable-python )
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DYGGDRASIL_DISABLE_PYTHON_C_API=1"
	    shift # past argument with no value
	    ;;
	--no-debug )
	    NO_DEBUG_MSG="TRUE"
	    shift # past argument with no value
	    ;;
	--local-rj )
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DRAPIDJSON_INCLUDE_DIRS=/Users/langmm/rapidjson/include"
	    shift # past argument with no value
	    ;;
	--config )
	    CONFIG_FLAGS="--config $2"
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DCMAKE_BUILD_TYPE=$2"
	    TEST_FLAGS="${TEST_FLAGS} -C $2"
	    shift
	    shift
	    ;;
	--speed )
	    SPEED_TEST="TRUE"
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
	-* | --* )
	    CMAKE_FLAGS="${CMAKE_FLAGS} $1"
	    shift
	    ;;
	*)
	    shift
	    ;;
    esac
done

if [ -n "$SPEED_TEST" ]; then
    NO_DEBUG_MSG="TRUE"
    DONT_TEST="TRUE"
fi

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
    if [ ! -n "$SPEED_TEST" ]; then
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
	if [ ! -n "$SPEED_TEST" ]; then
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_BUILD_CXX_TESTS=ON"
	fi
    else
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_CPP_LIBRARY=OFF"
    fi
    if [ -n "$DO_FORTRAN" ]; then
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_FORTRAN_LIBRARY=ON -DYGG_Fortran_REQUIRED=ON -DYGG_ENABLE_ELF=OFF"
	if [ ! -n "$SPEED_TEST" ]; then
	    CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DYGG_BUILD_FORTRAN_TESTS=ON"
	fi
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_Fortran=ON"
    else
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_FORTRAN_LIBRARY=OFF"
	CMAKE_FLAGS_SPEED="${CMAKE_FLAGS_SPEED} -DENABLE_Fortran=OFF"
    fi
    if [ -n "$DO_PYTHON" ]; then
	CMAKE_FLAGS_LIB="${CMAKE_FLAGS_LIB} -DBUILD_PYTHON_LIBRARY=ON"
	if [ ! -n "$SPEED_TEST" ]; then
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

if [ -n "$WITH_ASAN" ]; then
    export ASAN_OPTIONS=symbolize=1
    export ASAN_SYMBOLIZER_PATH=$(which llvm-symbolizer)
fi

if [ -n "$REBUILD" ]; then
    DONT_BUILD=""
    if [ -d "build" ]; then
	rm -rf "build"
    fi
    if [ -d "_skbuild" ]; then
	rm -rf "_skbuild"
    fi
    if [ -f "communication/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so" ]; then
	rm "communication/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so"
    fi
    if [ -f "communication/pyYggdrasil/lib/libYggInterface_py.dylib" ]; then
	rm "communication/pyYggdrasil/lib/libYggInterface_py.dylib"
    fi
fi
# export CMAKE_ARGS=${CMAKE_FLAGS}
if [ -n "$DO_PYTHON" ] && [ -n "$NO_CORE" ]; then
    export CMAKE_ARGS="${CMAKE_FLAGS} ${CMAKE_FLAGS_LIB}"
    if [ ! -n "$DONT_BUILD" ]; then
	python3 setup.py build_ext --inplace
    fi
    if [ -n "$WITH_ASAN" ]; then
	export DYLD_INSERT_LIBRARIES=$(clang -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
    fi
    cd python/pyYggdrasil
    export TEST_DIR=../test
    if [ ! -n "$DONT_TEST" ]; then
	export PYTHONFAULTHANDLER=1
	if [ -n "$WITH_LLDB" ]; then
	    lldb -o 'run' -o 'quit' -- $(which python3) -m pytest -svx $TEST_DIR
	else
	    python3 -m pytest -svx $TEST_DIR
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
	cmake --install . --prefix ../_install $CONFIG_FLAGS
    fi
    if [ ! -n "$DONT_TEST" ]; then
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

if [ -n "$SPEED_TEST" ]; then
    if [ -d "build_speed" ]; then
	rm -rf build_speed
    fi
    if [ ! -d "build_speed" ]; then
	mkdir build_speed
    fi
    export LD_LIBRARY_PATH="${LD_LIBRARY_PATH};$(pwd)/_install/lib"
    echo $LD_LIBRARY_PATH
    cd build_speed
    cmake ../test/speedtest -DCMAKE_PREFIX_PATH=../_install -DYggInterface_DIR=../../_install/lib/cmake/YggInterface -DN_MSG=$N_MSG -DS_MSG=$S_MSG -DCOMM=$COMM $CMAKE_FLAGS $CMAKE_FLAGS_SPEED
    cmake --build .
    if [ -n "$WITH_ASAN" ]; then
	export DYLD_INSERT_LIBRARIES=$(clang -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
    fi
    ctest $TEST_FLAGS
    # make test
    cd ..
fi

if [ -n "$DO_SYMBOLS" ]; then
    python utils/check_symbols.py build/test/unittest build/libYggInterface.dylib  &> symbols.txt
fi
