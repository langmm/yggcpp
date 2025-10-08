@setlocal

set "SKBUILD_CMAKE_ARGS=-G Ninja"
%PYTHON% -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation
if errorlevel 1 exit 1

mkdir conda_build
cd conda_build

cmake %CMAKE_ARGS% ^
      -G "Ninja" ^
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON ^
      -D "Python3_EXECUTABLE:FILEPATH=%PYTHON%" ^
      -D BUILD_CPP_LIBRARY:BOOL=ON ^
      -D BUILD_PYTHON_LIBRARY:BOOL=OFF ^
      -D BUILD_FORTRAN_LIBRARY:BOOL=OFF ^
      ..
if errorlevel 1 exit 1

cmake --install .
if errorlevel 1 exit 1

@endlocal
