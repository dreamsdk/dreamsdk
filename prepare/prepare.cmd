@echo off
set APP_TITLE=Source Packages Preparer for DreamSDK Setup
title %APP_TITLE%
cls

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set LOG_FILE=%BASE_DIR%\prepare.log
if exist %LOG_FILE% del %LOG_FILE%

call :log %APP_TITLE%
call :log

rem Read Configuration
rem set CONFIG_FILE=%BASE_DIR%\release.ini
rem for /F "tokens=*" %%i in (%CONFIG_FILE%) do (
rem 	set %%i 2> nul
rem )

rem Sanitize configuration entries
rem call :trim PATCH
rem call :trim PYTHON
rem call :trim INPUT_DIR
rem call :trim OUTPUT_DIR

rem Utilities
set SEVENZIP="C:\Program Files\7-Zip\7z.exe"
set PATCH="C:\DreamSDK\msys\1.0\bin\patch.exe"

set INPUT_DIR=%BASE_DIR%\..\..\setup-packages
set SYSTEM_OBJECTS_DIR=%BASE_DIR%\..\..\system-objects

set OUTPUT_DIR=%BASE_DIR%\..\..\setup\.sources

set BIN_PACKAGES_OUTPUT_DIR=%OUTPUT_DIR%\binary-packages

:start
pushd .

rem DEBUG Start
rem pause
rem goto system_objects
rem DEBUG end

:extract
call :log Extracting all setup packages...
if not exist "%BIN_PACKAGES_OUTPUT_DIR%" mkdir %BIN_PACKAGES_OUTPUT_DIR%
set NOT_INSTALLABLE_PACKAGE=0
set INSTALLABLE_PACKAGE=1

rem Extracting MinGW foundation base package
call :unpack %NOT_INSTALLABLE_PACKAGE% mingw-base 20200829

rem Extracting Toolchains...
call :unpack %INSTALLABLE_PACKAGE% arm-eabi arm-eabi-gcc-8.4.0-binutils-2.34 toolchain-experimental
call :unpack %INSTALLABLE_PACKAGE% sh-elf sh-elf-gcc-9.3.0-binutils-2.34-newlib-3.3.0 toolchain-experimental
call :unpack %INSTALLABLE_PACKAGE% arm-eabi arm-eabi-gcc-4.7.4-binutils-2.34 toolchain-stable
call :unpack %INSTALLABLE_PACKAGE% sh-elf sh-elf-gcc-4.7.4-binutils-2.34-newlib-2.0.0 toolchain-stable

rem Extracting GNU Debugger (GDB)...
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 no-python
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-2.7
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.3
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.4
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.5
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.6
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.7
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.8
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.9
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.10
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb 10.2 python-3.11

rem Extracting Addons Command-Line Tools
call :unpack %NOT_INSTALLABLE_PACKAGE% elevate 1.3 addons-cmd
call :unpack %NOT_INSTALLABLE_PACKAGE% pvr2png 1.01 addons-cmd
call :unpack %NOT_INSTALLABLE_PACKAGE% txfutils 20011020 addons-cmd
call :unpack %NOT_INSTALLABLE_PACKAGE% txfutils 20011020 addons-cmd txflib
call :unpack %NOT_INSTALLABLE_PACKAGE% vmutool 1.0 addons-cmd

rem Extracting Addons GUI Tools
call :unpack %NOT_INSTALLABLE_PACKAGE% bdreams 1.0.6c addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% buildsbi 3.2 addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% checker 2.0.3a addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% ipwriter 0.1.6a addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% ipwriter 0.1.6a addons-gui iplogos
call :unpack %NOT_INSTALLABLE_PACKAGE% mrwriter 0.3 addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% sbinducr 5.0 addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% vmutool 1.0 addons-gui

:system_objects
call :log Generating system objects...

call :copy "%SYSTEM_OBJECTS_DIR%\mingw" "%OUTPUT_DIR%\system-objects"
set SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR=%OUTPUT_DIR%\system-objects-configuration
if not exist %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR% mkdir %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR%

:profile
call :log * Generating profile file...
set SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR=%SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR%\etc
set SYSTEM_OBJECTS_PROFILE_INPUT_FILE=%OUTPUT_DIR%\mingw-base\msys\1.0\etc\profile
set SYSTEM_OBJECTS_PROFILE_OUTPUT_FILE=%SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR%\profile

if not exist %SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR% mkdir %SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR%

if not exist %SYSTEM_OBJECTS_PROFILE_OUTPUT_FILE% (
	if not exist %SYSTEM_OBJECTS_PROFILE_INPUT_FILE% (
		call :err File not found: "%SYSTEM_OBJECTS_PROFILE_INPUT_FILE%"
		goto end
	)
	move %SYSTEM_OBJECTS_PROFILE_INPUT_FILE% %SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR% >> %LOG_FILE% 2>&1
)
call :patch %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR% %SYSTEM_OBJECTS_DIR%\patches\etc.diff

:lib_embedded
call :log Checking embedded libraries...
if not exist "%OUTPUT_DIR%\lib-embedded" (
	call :err Missing embedded libraries. Please run Offline script.
	goto end
)

:source_packages
call :log Checking embedded source packages...
if not exist "%OUTPUT_DIR%\lib-embedded" (
	call :err Missing embedded source packages. Please run Offline script.
	goto end
)

goto end

:end
popd
pause
goto :EOF

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

:copy
set EXCLUDE_FILE=%BASE_DIR%\exclude.txt
echo .git\ > %EXCLUDE_FILE%
echo .svn\ >> %EXCLUDE_FILE%
xcopy %1\* %2 /exclude:%EXCLUDE_FILE% /s /i /y >> %LOG_FILE% 2>&1
if exist %EXCLUDE_FILE% del %EXCLUDE_FILE%
goto :EOF

:patch
%PATCH% -N -d %1 -p1 -r - < %2 >> %LOG_FILE% 2>&1
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

:unpack
set _installable_package=%1
set _base=%2
set _name=%2
set _ver=%3
if not "_%4"=="_" (
	set _base=%4\%_base%
	set _name=%2-%4
)
set _input=%INPUT_DIR%\%_base%\%3\%2-bin.7z
if not exist %_input% set _input=%INPUT_DIR%\%_base%\%3\%4-%2-bin.7z
set _output=%OUTPUT_DIR%\%_base%
if not exist %_input% (
	set _input=%INPUT_DIR%\%2\%3\%2-%4-bin.7z
	set _output=%OUTPUT_DIR%\%2\%2-%4
	set _name=%2-%4
)
if not "$%4"=="$" (
	if not "#%5"=="#" (
		set _input=%INPUT_DIR%\%_base%\%3\%2-%5-bin.7z
		set _output=%OUTPUT_DIR%\%4\%2-%5
		set _name=%2-%4::%5
	)
)
if exist %_input% (
	call :log * Unpacking %_name% ^(%_ver%^) ...
	%SEVENZIP% x "%_input%" -o"%_output%" -y >> %LOG_FILE% 2>&1
	if "$%_installable_package%"=="$1" (		
		copy %_input% %BIN_PACKAGES_OUTPUT_DIR% >> %LOG_FILE% 2>&1
	)
)
goto :EOF
