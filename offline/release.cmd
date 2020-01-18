@echo off
set APP_TITLE=KallistiOS Offline Packager for DreamSDK Setup
title %APP_TITLE%
cls

echo %APP_TITLE%
echo.

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\offline.ini
for /F "tokens=*" %%i in (%CONFIG_FILE%) do (
	set %%i 2> nul
)

rem Sanitize configuration entries
call :trim VERSION_KOS
call :trim VERSION_KOS_PORTS
call :trim VERSION_DCLOAD_IP
call :trim VERSION_DCLOAD_SERIAL
call :trim OFFLINE_HOME_DIR
call :trim PATCH
call :trim PYTHON

rem Directories
set HOME_BASE=%OFFLINE_HOME_DIR%\kos-base
set HOME_EXTRA=%OFFLINE_HOME_DIR%\kos-extra

rem Utilities
set PYREPL="%PYTHON%" "%BASE_DIR%\data\pyrepl.py"

pushd

rem KallistiOS
echo Processing: KallistiOS (%VERSION_KOS%)
set KOS_HOME_DIR=%HOME_BASE%\kos
set KOS_EXTRA_DIR=%HOME_EXTRA%\kos
%PYREPL% "KallistiOS ##version##" "KallistiOS %VERSION_KOS%-offline" "%KOS_HOME_DIR%" > nul
echo %VERSION_KOS% > %KOS_EXTRA_DIR%\OFFLINE

rem KallistiOS Ports
echo Processing: KallistiOS Ports (%VERSION_KOS_PORTS%)
set KOS_PORTS_DIR=%HOME_BASE%\kos-ports
set KOS_PORTS_EXTRA_DIR=%HOME_EXTRA%\kos-ports
set KOS_PORTS_PATCH_FILE=%BASE_DIR%\data\kos-ports.diff
%PATCH% -N -d %KOS_PORTS_DIR% -p1 -r - < %KOS_PORTS_PATCH_FILE% > nul
%PYREPL% "# kos-ports ##version##" "# kos-ports %VERSION_KOS_PORTS%-offline" "%KOS_PORTS_EXTRA_DIR%" > nul
echo %VERSION_KOS_PORTS% > %KOS_PORTS_EXTRA_DIR%\OFFLINE

rem Dreamcast-Tool IP
echo Processing: Dreamcast-Tool Internet Protocol (%VERSION_DCLOAD_IP%)
set EXTRA_DIR_DCLOAD_IP=%HOME_EXTRA%\dcload\dcload-ip
echo %VERSION_DCLOAD_IP% > %EXTRA_DIR_DCLOAD_IP%\OFFLINE

rem Dreamcast-Tool Serial
echo Processing: Dreamcast-Tool Serial (%VERSION_DCLOAD_SERIAL%)
set EXTRA_DIR_DCLOAD_SERIAL=%HOME_EXTRA%\dcload\dcload-serial
echo %VERSION_DCLOAD_SERIAL% > %EXTRA_DIR_DCLOAD_SERIAL%\OFFLINE

popd
echo.
pause
goto :EOF

:trim
rem Thanks to: https://stackoverflow.com/a/19686956/3726096
setlocal EnableDelayedExpansion
call :trimsub %%%1%%
endlocal & set %1=%tempvar%
goto :EOF

:trimsub
set tempvar=%*
goto :EOF
