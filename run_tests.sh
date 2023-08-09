set -e

DO_PYTHON=""
WITH_ASAN=""
ASAN_FLAGS=""

while [[ $# -gt 0 ]]; do
    case $1 in
	-p | --python )
	    DO_PYTHON="TRUE"
	    shift # past argument with no value
	    ;;
	--with-asan )
	    WITH_ASAN="TRUE"
	    shift # past argument with no value
	    ;;
	*)
	    ;;
    esac
done
	    
if [ -z "$DO_PYTHON" ]; then
    if [ -n "$WITH_ASAN" ]; then
	ASAN_FLAGS="-DYGG_BUILD_ASAN=ON -DYGG_BUILD_UBSAN=ON"
    fi
    if [ ! -d "build" ]; then
	mkdir build
    fi
    cd build
    cmake .. -DCMAKE_INSTALL_PREFIX=../devel -DYGG_ENABLE_COVERAGE=OFF -DYGG_SKIP_VALGRIND_TESTS=ON -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON -DRAPIDJSON_INCLUDE_DIRS=/Users/langmm/rapidjson/include -DYGG_BUILD_TESTS=ON -DBUILD_PYTHON_LIBRARY=OFF $ASAN_FLAGS
    make
    make test ARGS="--stop-on-failure"
    cd ../
else
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
	ASAN_FLAGS="--with-asan"
    fi
    python setup.py build_ext --inplace --rj-include-dir=../rapidjson/include/ $ASAN_FLAGS
    if [ -n "$WITH_ASAN" ]; then
	export DYLD_INSERT_LIBRARIES=$(clang -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
    fi
    cd communication/pyYggdrasil
    export PYTHONFAULTHANDLER=1
    python-crash -m pytest -sv ../../test/
    # Copy test commands to test/script.py
    # cp ../../test/script.py ./
    # lldb python script.py  # r to run
    cd ../../
fi


