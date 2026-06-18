@setlocal EnableDelayedExpansion
@echo on

powershell -command "Expand-Archive -Path utils\objconv.zip -DestinationPath .\ -Verbose"
powershell -command "Get-ChildItem -Path .\"

mkdir conda_build

rem set "CMAKE_INSTALL_PREFIX=%PREFIX%"
cmake -B conda_build -S %SRC_DIR% ^
      -G Ninja ^
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON ^
      -D CMAKE_MESSAGE_LOG_LEVEL:STRING=DEBUG ^
      -D CMAKE_BUILD_PARALLEL_LEVEL=1 ^
      -D BUILD_CXX_LIBRARY:BOOL=ON ^
      -D BUILD_Python_LIBRARY:BOOL=OFF ^
      -D BUILD_Fortran_LIBRARY:BOOL=ON ^
      -D YGG_CXX_REQUIRED:BOOL=ON ^
      -D YGG_Fortran_REQUIRED:BOOL=ON ^
      -D YGGINTERFACE_VERSION=%PKG_VERSION% ^
      -D "Python3_EXECUTABLE:FILEPATH=%PYTHON%" ^
      %CMAKE_ARGS% || goto :error
cmake --build conda_build -j%CPU_COUNT% || goto :error
rem cmake --build conda_build || goto :error
cmake --install conda_build --prefix %PREFIX% || goto :error

goto :eof

:error
echo Failed with error #%errorlevel%.
exit 1

@endlocal
