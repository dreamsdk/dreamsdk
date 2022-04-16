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
if not exist %SETUP_INPUT_DIR% goto err_input_dir

:check_iscc
if not exist %ISCC% goto err_binary_iscc

:start
pushd .

:exec_iscc
rem Generate Setup program
call :log Generating DreamSDK Setup...
set SETUP_SCRIPT_FILE="%SETUP_INPUT_DIR%\src\dreamsdk.iss"
set DUALSIGN_SCRIPT_FILE="%BASE_DIR%\..\..\embedded\dualsign\dualsign.cmd"
%ISCC% %SETUP_SCRIPT_FILE% "/SSignTool=%DUALSIGN_SCRIPT_FILE% $p" >> %LOG_FILE% 2>&1

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
