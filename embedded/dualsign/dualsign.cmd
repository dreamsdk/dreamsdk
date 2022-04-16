@echo off

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%
set TARGET_FILE=%1

:init
rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\dualsign.ini
if not exist "%CONFIG_FILE%" goto err_config
for /f "tokens=*" %%i in (%CONFIG_FILE%) do (
  set %%i 2> nul
  rem Sanitize configuration entry
  for /f "tokens=1 delims==" %%j in ("%%i") do (
    call :trim %%j
  )
)

:check
if "%TARGET_FILE%$"=="$" goto err_inputfile
if not exist %TARGET_FILE% goto err_inputfile
if not exist %SIGNTOOL% goto err_binary_signtool
if not exist %CERTIFICATE_FILE% goto err_certificate
if "%TIMESTAMP_SERVER_URL%$"=="$" goto err_timestamp_server

:start
pushd .

:exec
%SIGNTOOL% sign /f %CERTIFICATE_FILE% /fd sha1 /tr %TIMESTAMP_SERVER_URL% /td sha256 /p %CERTIFICATE_PASSWORD% /v %TARGET_FILE%
%SIGNTOOL% sign /f %CERTIFICATE_FILE% /fd sha256 /tr %TIMESTAMP_SERVER_URL% /td sha256 /p %CERTIFICATE_PASSWORD% /v /as %TARGET_FILE%

:finish
goto end

:end
popd
goto :EOF

rem ## Errors ##################################################################

:err_config
echo The configuration file was not found.
echo File: "%CONFIG_FILE%"
goto end

:err_inputfile
echo The specified input file was not found.
echo File: "%TARGET_FILE%"
goto end

:err_certificate
echo The certificate file was not found.
echo File: "%CERTIFICATE_FILE%"
goto end

:err_binary_signtool
echo Sign Tool was not found.
echo File: "%SIGNTOOL%"
goto end

:err_timestamp_server
echo Timestamp Server URL is not defined.
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
