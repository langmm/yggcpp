setlocal EnableDelayedExpansion

IF NOT DEFINED ENABLE_CXX (set ENABLE_CXX=ON)
IF NOT DEFINED ENABLE_Fortran (set ENABLE_Fortran=ON)
IF NOT DEFINED ENABLE_Python (set ENABLE_Python=OFF)
IF NOT DEFINED ENABLE_RMQ (set ENABLE_RMQ=OFF)

set "builddir=build_speed"
if not exist "!builddir!" mkdir "!builddir!"
if !errorlevel! neq 0 exit /b !errorlevel!
cd "!builddir!"
cmake -G Ninja ^
      -D ENABLE_C:BOOL=%ENABLE_CXX% ^
      -D ENABLE_CXX:BOOL=%ENABLE_CXX% ^
      -D ENABLE_Fortran:BOOL=%ENABLE_Fortran% ^
      -D ENABLE_Python:BOOL=%ENABLE_Python% ^
      -D ENABLE_RMQ:BOOL=%ENABLE_RMQ% ^
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
