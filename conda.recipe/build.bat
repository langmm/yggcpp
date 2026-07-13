@setlocal EnableDelayedExpansion
@echo on

IF NOT DEFINED ENABLE_CXX (set ENABLE_CXX=ON)
IF NOT DEFINED ENABLE_Fortran (set ENABLE_Fortran=ON)
IF NOT DEFINED ENABLE_Python (set ENABLE_Python=OFF)
IF NOT DEFINED STANDALONE_Fortran (set STANDALONE_Fortran=OFF)
IF NOT DEFINED STANDALONE_Fortran_LINK_EXISTING_CXX (set STANDALONE_Fortran_LINK_EXISTING_CXX=OFF)

powershell -command "Expand-Archive -Path utils\objconv.zip -DestinationPath .\ -Verbose"
powershell -command "Get-ChildItem -Path .\"

mkdir conda_build

rem The following is set in CMAKE_ARGS by vc conda package during
rem   activation if CONDA_BUILD == 1:
rem     "CMAKE_INSTALL_PREFIX=%PREFIX%\Library"
IF NOT DEFINED CMAKE_INSTALL_PREFIX (set CMAKE_INSTALL_PREFIX=%PREFIX%\Library)
cmake -B conda_build -S %SRC_DIR% ^
      -G Ninja ^
      -D YGG_DEBUG_LEVEL=5 ^
      -D CMAKE_VERBOSE_MAKEFILE:BOOL=ON ^
      -D CMAKE_MESSAGE_LOG_LEVEL:STRING=DEBUG ^
      -D CMAKE_BUILD_PARALLEL_LEVEL=1 ^
      -D BUILD_CXX_LIBRARY:BOOL=%ENABLE_CXX% ^
      -D BUILD_Fortran_LIBRARY:BOOL=%ENABLE_Fortran% ^
      -D BUILD_Python_LIBRARY:BOOL=%ENABLE_Python% ^
      -D ENABLE_REST:BOOL=OFF ^
      -D YGG_Fortran_STANDALONE:BOOL=%STANDALONE_Fortran% ^
      -D YGG_Fortran_STANDALONE_LINK_EXISTING_CXX:BOOL=%STANDALONE_Fortran_LINK_EXISTING_CXX% ^
      -D YGG_CXX_REQUIRED:BOOL=%ENABLE_CXX% ^
      -D YGG_Fortran_REQUIRED:BOOL=%ENABLE_Fortran% ^
      -D YGGINTERFACE_VERSION=%PKG_VERSION% ^
      -D "Python3_EXECUTABLE:FILEPATH=%PYTHON%" ^
      %CMAKE_ARGS% || goto :error
cmake --build conda_build -j%CPU_COUNT% || goto :error
rem cmake --build conda_build || goto :error
cmake --install conda_build || goto :error

goto :eof

:error
echo Failed with error #%errorlevel%.
exit 1

@endlocal
