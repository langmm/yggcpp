set -e

LANGUAGE="CXX"
REBUILD_LIB=""
REBUILD=""
N_MSG="100"
S_MSG="100"
COMM="DEFAULT_COMM"

while [[ $# -gt 0 ]]; do
    case $1 in
	-l | --language )
	    LANGUAGE="$2"
	    shift
	    shift
	    ;;
	--rebuild )
	    REBUILD="TRUE"
	    shift
	    ;;
	--rebuild-lib )
	    REBUILD_LIB="TRUE"
	    shift
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
	-c | --comm )
	    COMM="$2"
	    shift
	    shift
	    ;;
	*)
	    ;;
    esac
done

if [ "$LANGUAGE" = "PYTHON" ]; then
    if [ -n "$REBUILD_LIB" ]; then
	if [ -d "_skbuild" ]; then
	    rm -rf "_skbuild"
	fi
	if [ -f "communication/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so" ]; then
	    rm "communication/pyYggdrasil/pyYggdrasil.cpython-39-darwin.so"
	fi
	if [ -f "communication/pyYggdrasil/lib/libYggInterface_py.dylib" ]; then
	    rm "communication/pyYggdrasil/lib/libYggInterface_py.dylib"
	fi
	pip3 install . -v
    fi
else
    if [ -n "$REBUILD_LIB" ]; then
	if [ -d "build" ]; then
	    rm -rf build
	fi
    fi
    if [ ! -d "build" ]; then
	mkdir build
    fi
    cd build
    cmake .. -DBUILD_FORTRAN_LIBRARY=ON
    cmake --build .
    cmake --install . --prefix ../devel
    cd ..
fi

if [ -n "$REBUILD" ]; then
    if [ -d "build_speed" ]; then
	rm -rf build_speed
    fi
fi
if [ ! -d "build_speed" ]; then
    mkdir build_speed
fi
cd build_speed
cmake ../test/speedtest -DCMAKE_PREFIX_PATH=../devel -DCMAKE_INSTALL_PREFIX=. -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON -DYggInterface_DIR=../../../devel/lib/cmake/YggInterface -DLANGUAGE=$LANGUAGE -DN_MSG=$N_MSG -DS_MSG=$S_MSG -DCOMM=$COMM
make
make test
