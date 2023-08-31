set -e

DO_PYTHON=""
WITH_ASAN=""
DONT_BUILD=""
NO_CORE=""
CMAKE_FLAGS="" # -DRAPIDJSON_INCLUDE_DIRS=/Users/langmm/rapidjson/include"
WITH_LLDB=""

while [[ $# -gt 0 ]]; do
    case $1 in
	-p | --python )
	    DO_PYTHON="TRUE"
	    shift # past argument with no value
	    ;;
	--with-asan )
	    WITH_ASAN="TRUE"
	    CMAKE_FLAGS="${CMAKE_FLAGS} -DYGG_BUILD_ASAN=ON -DYGG_BUILD_UBSAN=ON"
	    shift # past argument with no value
	    ;;
	--dont-build )
	    DONT_BUILD="TRUE"
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
	*)
	    ;;
    esac
done
	    
if [ -z "$DO_PYTHON" ]; then
    if [ ! -d "build" ]; then
	mkdir build
    fi
    cd build
    cmake .. -DCMAKE_INSTALL_PREFIX=../devel -DYGG_ENABLE_COVERAGE=OFF -DYGG_SKIP_VALGRIND_TESTS=ON -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON -DYGG_BUILD_TESTS=ON -DBUILD_PYTHON_LIBRARY=OFF $CMAKE_FLAGS
    make
    make test ARGS="--stop-on-failure"
    cd ../
else
    if [ -n "$DONT_BUILD"]; then
	if [ -d "_skbuild" ]; then
	    rm -rf "_skbuild"
	fi
	if [ -f "communication/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so" ]; then
	    rm "communication/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so"
	fi
	if [ -f "communication/pyYggdrasil/lib/libYggInterface_py.dylib" ]; then
	    rm "communication/pyYggdrasil/lib/libYggInterface_py.dylib"
	fi
	
	if [ -n "$WITH_ASAN" ]; then
	    export ASAN_OPTIONS=symbolize=1
	    export ASAN_SYMBOLIZER_PATH=$(which llvm-symbolizer)
	fi
	export CMAKE_ARGS=${CMAKE_FLAGS}
	if [ -n "$NO_CORE" ]; then
	    python3 setup.py build_ext --inplace
	else
	    pip3 install . -v
	fi
    fi
    if [ -n "$WITH_ASAN" ]; then
	export DYLD_INSERT_LIBRARIES=$(clang -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
    fi
    if [ -n "$NO_CORE" ]; then
	cd python/pyYggdrasil
	export TEST_DIR=../test
    else
	export TEST_DIR=python/test
    fi
    export PYTHONFAULTHANDLER=1
    if [ -n "$WITH_LLDB" ]; then
	lldb -o 'run' -o 'quit' -- $(which python3) -m pytest -svx $TEST_DIR
    else
	python3 -m pytest -svx $TEST_DIR
    fi
    if [ -n "$NO_CORE" ]; then
	cd ../../
    fi
fi


