setlocal EnableDelayedExpansion

if not exist build_speed mkdir build_speed
if errorlevel neq 0 exit /b errorlevel

cd build_speed
cmake -G "Ninja" ..\\tests\\speedtest
if errorlevel neq 0 exit /b errorlevel

cmake --build . --config Release
if errorlevel neq 0 exit /b errorlevel

ctest
if errorlevel neq 0 exit /b errorlevel

cd ..

@endlocal
