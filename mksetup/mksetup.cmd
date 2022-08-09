@echo off
set APP_TITLE=DreamSDK Setup Generator
title %APP_TITLE%
cls

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set LOG_FILE=%BASE_DIR%\mksetup.log
if exist %LOG_FILE% del %LOG_FILE%

call :log %APP_TITLE%
call :log

:init
rem Global boolean variable used in various locations
set FUNC_RESULT=0

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\mksetup.ini
if not exist "%CONFIG_FILE%" goto err_config
for /f "tokens=*" %%i in (%CONFIG_FILE%) do (
  set %%i 2> nul
  rem Sanitize configuration entry
  for /f "tokens=1 delims==" %%j in ("%%i") do (
    call :trim %%j
  )
)

:check_input_dir
rem Input Directory
call :checkdir FUNC_RESULT %SETUP_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

:check_iscc
call :checkfile FUNC_RESULT %ISCC%
if "+%FUNC_RESULT%"=="+0" goto err_binary_iscc

:start
pushd .

:exec_iscc
rem Generate Setup program
call :log Generating DreamSDK Setup...
set SETUP_SCRIPT_FILE="%SETUP_INPUT_DIR%\src\dreamsdk.iss"
set DUALSIGN_SCRIPT_FILE="%SETUP_HELPERS_INPUT_DIR%\dualsign\dualsign.cmd"
%ISCC% %SETUP_SCRIPT_FILE% "/SSignTool=%DUALSIGN_SCRIPT_FILE% $p" >> %LOG_FILE% 2>&1

:check_output_file
set SETUP_OUTPUT_FILE="%SETUP_INPUT_DIR%\bin\setup.exe"
call :checkfile FUNC_RESULT %SETUP_OUTPUT_FILE%
if "+%FUNC_RESULT%"=="+0" goto err_output_generation

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

:err_binary_iscc
call :err Inno Setup Compiler was not found.
call :log File: "%ISCC%"
goto end

:err_output_generation
call :err Unable to generate the Setup.
call :log Please check the Log file to learn more.
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

:checkdir
setlocal EnableDelayedExpansion
set _dirname=%2
set _direxist=0
if [%_dirname%]==[] goto checkdir_exit
if not exist %_dirname% mkdir %_dirname%
if exist %_dirname% set _direxist=1
:checkdir_exit
endlocal & set "%~1=%_direxist%"
goto :EOF

:checkfile
setlocal EnableDelayedExpansion
set _filepath=%2
set _fileexist=0
if [%_filepath%]==[] goto checkfile_exit
if exist %_filepath% set _fileexist=1
if "$%_fileexist%"=="$0" (
  call :check_command %_filepath% _fileexist
)
:checkfile_exit
endlocal & set "%~1=%_fileexist%"
goto :EOF
