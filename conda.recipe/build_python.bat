@setlocal EnableDelayedExpansion
@echo on

powershell -command "Expand-Archive -Path utils\objconv.zip -DestinationPath .\ -Verbose"
powershell -command "Get-ChildItem -Path .\"

set "SKBUILD_CMAKE_ARGS=-G Ninja -DPython3_EXECUTABLE:FILEPATH=%PYTHON%"
%PYTHON% -m pip install . --no-deps --ignore-installed -vvv --no-build-isolation || goto :error

goto :eof

:error
echo Failed with error #%errorlevel%.
exit 1

@endlocal
