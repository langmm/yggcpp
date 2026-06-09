setlocal EnableDelayedExpansion

set "speeddir=tests\\speedtest"
set "builddir=build_speed"
if not exist "!builddir!" mkdir "!builddir!"
if !errorlevel! neq 0 exit /b !errorlevel!
cd "!builddir!"
cmake -G "Ninja" ^
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON ^
      "!speeddir!"
if !errorlevel! neq 0 exit /b !errorlevel!
cmake --build . --config Release
if !errorlevel! neq 0 exit /b !errorlevel!

ctest
if !errorlevel! neq 0 exit /b !errorlevel!

cd ..

@endlocal
