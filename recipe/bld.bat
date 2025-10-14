@setlocal

set "SKBUILD_CMAKE_ARGS=-G Ninja"
%PYTHON% -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation
if errorlevel 1 exit 1

mkdir conda_build
cd conda_build

cmake %CMAKE_ARGS% ^
      -G "Ninja" ^
      -D VERBOSE:BOOL=ON ^
      -D "Python3_EXECUTABLE:FILEPATH=%PYTHON%" ^
      -D BUILD_CXX_LIBRARY:BOOL=ON ^
      -D BUILD_Python_LIBRARY:BOOL=OFF ^
      -D BUILD_Fortran_LIBRARY:BOOL=ON ^
      -D YGG_CXX_REQUIRED:BOOL=ON ^
      -D YGG_Fortran_REQUIRED:BOOL=ON ^
      ..
if errorlevel 1 exit 1

cmake --build .
if errorlevel 1 exit 1

cmake --install .
if errorlevel 1 exit 1

@endlocal
