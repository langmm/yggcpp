set -e
cmake .. -DCMAKE_INSTALL_PREFIX=../devel -DYGG_ENABLE_COVERAGE=OFF -DYGG_SKIP_VALGRIND_TESTS=ON
make
make test ARGS="--stop-on-failure"
