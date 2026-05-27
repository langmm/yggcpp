@setlocal EnableDelayedExpansion
@echo on

mkdir conda_build

cmake -B conda_build -S %SRC_DIR% ^
      -G "Ninja" ^
      -D BUILD_CXX_LIBRARY:BOOL=ON ^
      -D BUILD_Python_LIBRARY:BOOL=OFF ^
      -D BUILD_Fortran_LIBRARY:BOOL=ON ^
      -D YGG_CXX_REQUIRED:BOOL=ON ^
      -D YGG_Fortran_REQUIRED:BOOL=ON ^
      -D CMAKE_BUILD_PARALLEL_LEVEL=1 ^
      -D VERBOSE:BOOL=ON ^
      -D "Python3_EXECUTABLE:FILEPATH=%PYTHON%" ^
      %CMAKE_ARGS% || goto :error
cmake --build conda_build -j%CPU_COUNT% || goto :error
cmake --install conda_build || goto :error

set "SKBUILD_CMAKE_ARGS=-G Ninja"
%PYTHON% -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation || goto :error

goto :eof

:error
echo Failed with error #%errorlevel%.
exit 1

@endlocal
