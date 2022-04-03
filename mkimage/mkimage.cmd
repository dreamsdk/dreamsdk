rem @echo off
set APP_TITLE=DreamSDK Setup Image Builder
title %APP_TITLE%
cls

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set LOG_FILE=%BASE_DIR%\mkimage.log
if exist %LOG_FILE% del %LOG_FILE%

call :log %APP_TITLE%
call :log

:init
rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\mkimage.ini
if not exist "%CONFIG_FILE%" goto err_config
for /f "tokens=*" %%i in (%CONFIG_FILE%) do (
  set %%i 2> nul
  rem Sanitize configuration entry
  for /f "tokens=1 delims==" %%j in ("%%i") do (
    call :trim %%j
  )
)

rem Some temp directory and files
set SETUP_SOURCE_DIR=%SETUP_INPUT_DIR%\bin
set TEMP_RESULT_FILE=%TEMP%\~mkimage.tmp

rem Utilities
set MKISOFS="%DREAMSDK_HOME%\msys\1.0\bin\mkisofs.exe"
set CDI4DC="%DREAMSDK_HOME%\msys\1.0\bin\cdi4dc.exe"
set GETVER="%PYTHON%" "%BASE_DIR%\data\getver.py" "%SETUP_SOURCE_DIR%\setup.exe"

:check_input_dir
rem Input Directory
if not exist %SETUP_INPUT_DIR% goto err_input_dir

:check_output_dir
rem Output Directory
set SETUP_OUTPUT_DIR=%BASE_DIR%\bin
if not exist %SETUP_OUTPUT_DIR% mkdir %SETUP_OUTPUT_DIR%

:check_tools
if not exist %MKISOFS% goto err_binary_mkisofs
if not exist %CDI4DC% goto err_binary_cdi4dc

:check_python
set PYTHON_VERSION_MAJOR=
set PYTHON_VERSION=
call :get_version_python PYTHON_VERSION_MAJOR PYTHON_VERSION
if "$%PYTHON_VERSION_MAJOR%"=="$3" goto start
goto err_binary_python

:start
pushd .

:extract_versioninfo
rem Extract product information from the Setup file...
%GETVER% ProductVersion > %TEMP_RESULT_FILE%
set /p PACKAGE_RELEASE_VERSION=< %TEMP_RESULT_FILE%
 
rem Extract product name from the Setup file...
%GETVER% ProductName > %TEMP_RESULT_FILE%
set /p PACKAGE_NAME=< %TEMP_RESULT_FILE%

rem Extract company from the Setup file...
%GETVER% CompanyName > %TEMP_RESULT_FILE%
set /p PUBLISHER=< %TEMP_RESULT_FILE%

if exist %TEMP_RESULT_FILE% del %TEMP_RESULT_FILE%

:setting_up_parameters
rem Settings for final Setup image...
set SYSID=Win32
set PREPARER=%PACKAGE_NAME% Setup Generator
set APPID=%PACKAGE_NAME% %PACKAGE_RELEASE_VERSION%
set VOLUMEID=%PACKAGE_NAME%_%PACKAGE_RELEASE_VERSION%

rem Output files
set SETUP_OUTPUT_ISO_FILE=DreamSDK-%PACKAGE_RELEASE_VERSION%-Setup.iso

:make_autorun_inf
set AUTORUN_INF=%SETUP_SOURCE_DIR%\autorun.inf
echo [autorun] > %AUTORUN_INF%
echo icon=setup.exe >> %AUTORUN_INF%
echo open=setup.exe >> %AUTORUN_INF%
echo label=%APPID% >> %AUTORUN_INF%

:generate_iso
rem Generate Setup program
call :log Generating Final ISO Image...
%MKISOFS% -V "%VOLUMEID%" -sysid "%SYSID%" -publisher "%PUBLISHER%" -preparer "%PREPARER%" -appid "%APPID%" -duplicates-once -joliet -rational-rock -full-iso9660-filenames -o "%SETUP_OUTPUT_DIR%\%SETUP_OUTPUT_ISO_FILE%" "%SETUP_SOURCE_DIR%" >> %LOG_FILE% 2>&1
if "%errorlevel%+"=="0+" goto finish
goto err_generation

:finish
call :log
call :log Done!
call :log
goto end

:end
popd
pause
goto :EOF

rem ## Errors ##################################################################

:err_config
call :err The configuration file was not found.
call :log File: "%CONFIG_FILE%"
goto end

:err_input_dir
call :err The specified input directory (SETUP_INPUT_DIR) was not found.
call :log Directory: "%SETUP_INPUT_DIR%"
goto end

:err_binary_mkisofs
call :err Make ISO Image File System (mkisofs) was not found.
call :log Do you have DreamSDK installed?
call :log File: "%MKISOFS%"
goto end

:err_binary_cdi4dc
call :err CDI4DC was not found.
call :log Do you have DreamSDK installed?
call :log File: "%CDI4DC%"
goto end

:err_binary_python
call :err Python 3 was not found.
call :log File: "%PYTHON%"
goto end

:err_generation
call :err Unable to generate the ISO file.
call :log File: "%SETUP_OUTPUT_ISO_FILE%"
goto end

rem ## Utilities ###############################################################

:trim
rem Thanks to: https://stackoverflow.com/a/19686956/3726096
setlocal EnableDelayedExpansion
call :trimsub %%%1%%
endlocal & set %1=%tempvar%
goto :EOF
:trimsub
set tempvar=%*
goto :EOF

:warn
call :log WARNING: %*
goto :EOF

:err
call :log ERROR: %*
goto :EOF

:log
set tmplog=%*
if "%tmplog%"=="" goto logempty
echo %tmplog%
echo %tmplog%>> %LOG_FILE% 2>&1
goto :EOF
:logempty
echo.
echo.>> %LOG_FILE% 2>&1
goto :EOF

:get_version_python
rem Check if Python is installed and retrieve the version (incl. Major Version)
rem Usage: call :get_version_python PYTHON_VERSION_MAJOR PYTHON_VERSION
setlocal EnableDelayedExpansion
set _python_exec=%PYTHON%
set _python_installed=0
set _python_version_major=
set _python_version=
set _python_buffer_temp=pythonver.tmp
if exist %_python_exec% goto get_version_python_check
call :check_command %_python_exec% _python_installed
if "%_python_installed%"=="0" goto get_version_python_exit
:get_version_python_check
%_python_exec% --version > %_python_buffer_temp% 2>&1
set /p _cmd_raw_output=<%_python_buffer_temp%
if "%_cmd_raw_output:~0,6%"=="Python" goto get_version_python_install
:get_version_python_install
for /f "tokens=2 delims= " %%V in ('type %_python_buffer_temp%') do (
  set _python_version=%%V
)
set "_python_version_major=%_python_version:~0,1%"
:get_version_python_exit
if exist %_python_buffer_temp% del %_python_buffer_temp%
endlocal & (
	set "%~1=%_python_version_major%"
	set "%~2=%_python_version%"
)
goto :EOF

:check_command
rem Thanks Joey: https://superuser.com/a/175831
rem Warning: _exec should be the name of the executable to check WITH its extension (e.g., ".exe")
setlocal EnableDelayedExpansion
set _exec=%1
set _cmdfound=0
for %%x in (%_exec%) do if not [%%~$PATH:x]==[] set _cmdfound=1
if "%_cmdfound%"=="1" goto check_command_exit
rem Try with the ".exe" extension
set _exec=%_exec%.exe
for %%x in (%_exec%) do if not [%%~$PATH:x]==[] set _cmdfound=1
:check_command_exit
endlocal & set "%~2=%_cmdfound%"
goto :EOF
