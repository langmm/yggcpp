@setlocal

set "SKBUILD_CMAKE_ARGS=-G Ninja"
%PYTHON% -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation
if errorlevel 1 exit 1

rem mkdir conda_build
rem cd conda_build

rem cmake %CMAKE_ARGS% ^
rem       -G "Ninja" ^
rem       -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON ^
rem       -D "Python3_EXECUTABLE:FILEPATH=%PYTHON%" ^
rem       -D BUILD_CPP_LIBRARY:BOOL=ON ^
rem       -D BUILD_PYTHON_LIBRARY:BOOL=ON ^
rem       -D YGG_Fortran_REQUIRED:BOOL=ON ^
rem       ..
rem if errorlevel 1 exit 1

rem cmake --install .
rem if errorlevel 1 exit 1

@endlocal
