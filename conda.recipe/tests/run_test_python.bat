setlocal EnableDelayedExpansion

set "builddir=build_speed"
if not exist "!builddir!" mkdir "!builddir!"
if !errorlevel! neq 0 exit /b !errorlevel!
cd "!builddir!"
cmake -G Ninja ^
      -D ENABLE_C:BOOL=OFF ^
      -D ENABLE_CXX:BOOL=OFF ^
      -D ENABLE_Fortran:BOOL=OFF ^
      -D ENABLE_Python:BOOL=ON ^
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON ^
      -D CMAKE_MESSAGE_LOG_LEVEL:STRING=DEBUG ^
      -D CMAKE_BUILD_PARALLEL_LEVEL=1 ^
      ..\\tests\\speedtest
if !errorlevel! neq 0 exit /b !errorlevel!
cmake --build . --config Release
if !errorlevel! neq 0 exit /b !errorlevel!

ctest
if !errorlevel! neq 0 exit /b !errorlevel!

cd ..

@endlocal
