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

rem Check if DreamSDK is installed (of course, you can use a previous version!)
if "$%DREAMSDK_HOME%"=="$" goto err_dreamsdk_missing

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\prepare.template.ini
if not exist "%CONFIG_FILE%" goto err_config
for /f "tokens=*" %%i in (%CONFIG_FILE%) do (
  set %%i 2> nul
  rem Sanitize configuration entry
  for /f "tokens=1 delims==" %%j in ("%%i") do (
    call :trim %%j
  )
)

rem Utilities
set PATCH="%DREAMSDK_HOME%\msys\1.0\bin\patch.exe"
::set RUNNER="%DREAMSDK_HOME%\msys\1.0\opt\dreamsdk\dreamsdk-runner.exe"

rem Input Directory
if not exist "%CODEBLOCKS_PATCHER_INPUT_DIR%" goto err_input_dir
if not exist "%DOCUMENTATION_INPUT_DIR%" goto err_input_dir
if not exist "%HELPERS_INPUT_DIR%" goto err_input_dir
if not exist "%MANAGER_INPUT_DIR%" goto err_input_dir
if not exist "%RUNNER_INPUT_DIR%" goto err_input_dir
if not exist "%SETUP_PACKAGES_INPUT_DIR%" goto err_input_dir
if not exist "%SHELL_INPUT_DIR%" goto err_input_dir
if not exist "%SYSTEM_OBJECTS_INPUT_DIR%" goto err_input_dir

rem Output Directory
if not exist "%SETUP_OUTPUT_DIR%" goto err_output_dir

set OUTPUT_DIR=%SETUP_OUTPUT_DIR%\.sources
if not exist "%OUTPUT_DIR%" mkdir %OUTPUT_DIR%

set BIN_OUTPUT_DIR=%OUTPUT_DIR%\dreamsdk-binaries
if not exist "%BIN_OUTPUT_DIR%" mkdir %BIN_OUTPUT_DIR%

set BIN_PACKAGES_OUTPUT_DIR=%OUTPUT_DIR%\binary-packages

:start
pushd .

:lib_embedded
call :log Checking embedded libraries...
if not exist "%OUTPUT_DIR%\lib-embedded" goto err_offline

:source_packages
call :log Checking embedded source packages...
if not exist "%OUTPUT_DIR%\source-packages" goto err_offline

:extract
call :log Extracting packages...
if not exist "%BIN_PACKAGES_OUTPUT_DIR%" mkdir %BIN_PACKAGES_OUTPUT_DIR%

set NOT_INSTALLABLE_PACKAGE=0
set INSTALLABLE_PACKAGE=1
set INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT=2
set NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT=3

rem Extracting MinGW foundation base package
call :unpack %NOT_INSTALLABLE_PACKAGE% mingw-base %MINGW_BASE_VERSION%

rem Extracting MSYS packages
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% curl %MSYS_BASE_CURL_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% gawk %MSYS_BASE_GAWK_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% libjpeg %MSYS_BASE_LIBJPEG_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% libpng %MSYS_BASE_LIBPNG_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% mintty %MSYS_BASE_MINTTY_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% msys-core-extended %MSYS_BASE_CORE_EXTENDED_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% wget %MSYS_BASE_WGET_VERSION% msys-base

rem Extracting Toolchains...
call :unpack %INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% arm-eabi %TOOLCHAIN_EXPERIMENTAL_ARM_EABI_VERSION% toolchain-experimental
call :unpack %INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% sh-elf %TOOLCHAIN_EXPERIMENTAL_SH_ELF_VERSION% toolchain-experimental
call :unpack %INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% arm-eabi %TOOLCHAIN_STABLE_ARM_EABI_VERSION% toolchain-stable
call :unpack %INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% sh-elf %TOOLCHAIN_STABLE_SH_ELF_VERSION% toolchain-stable

rem Extracting GNU Debugger (GDB)...
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% no-python
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-2.7
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.0
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.1
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.2
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.3
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.4
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.5
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.6
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.7
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.8
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.9
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.10
call :unpack %INSTALLABLE_PACKAGE% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.11

rem Extracting Addons Command-Line Tools
call :unpack %NOT_INSTALLABLE_PACKAGE% elevate %ADDONS_CMD_ELEVATE_VERSION% addons-cmd
call :unpack %NOT_INSTALLABLE_PACKAGE% pvr2png %ADDONS_CMD_PVR2PNG_VERSION% addons-cmd
call :unpack %NOT_INSTALLABLE_PACKAGE% txfutils %ADDONS_CMD_TXFUTILS_VERSION% addons-cmd
call :unpack %NOT_INSTALLABLE_PACKAGE% txfutils %ADDONS_CMD_TXFUTILS_VERSION% addons-cmd txflib
call :unpack %NOT_INSTALLABLE_PACKAGE% vmutool %ADDONS_CMD_VMUTOOL_VERSION% addons-cmd

rem Extracting Addons GUI Tools
call :unpack %NOT_INSTALLABLE_PACKAGE% bdreams %ADDONS_GUI_BDREAMS_VERSION% addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% buildsbi %ADDONS_GUI_BUILDSBI_VERSION% addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% checker %ADDONS_GUI_CHECKER_VERSION% addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% ipwriter %ADDONS_GUI_IPWRITER_VERSION% addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% ipwriter %ADDONS_GUI_IPWRITER_VERSION% addons-gui iplogos
call :unpack %NOT_INSTALLABLE_PACKAGE% mrwriter %ADDONS_GUI_MRWRITER_VERSION% addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% sbinducr %ADDONS_GUI_SBINDUCR_VERSION% addons-gui
call :unpack %NOT_INSTALLABLE_PACKAGE% vmutool %ADDONS_GUI_VMUTOOL_VERSION% addons-gui

:system_objects
call :log Generating system objects...

call :copy "%SYSTEM_OBJECTS_INPUT_DIR%\mingw" "%OUTPUT_DIR%\system-objects"
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
call :patch %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR% %SYSTEM_OBJECTS_INPUT_DIR%\patches\etc.diff

:setup_helpers
call :log Copying Setup Helpers...
set SETUP_HELPERS_OUTPUT_DIR=%SETUP_OUTPUT_DIR%\.helpers
if not exist "%SETUP_HELPERS_OUTPUT_DIR%" mkdir %SETUP_HELPERS_OUTPUT_DIR%
call :copybinary cbhelper %BASE_DIR%\..\embedded %SETUP_HELPERS_OUTPUT_DIR%
call :copybinary pecheck %BASE_DIR%\..\embedded %SETUP_HELPERS_OUTPUT_DIR%

:dreamsdk_helpers
call :log Copying Helpers...
set HELPERS_OUTPUT_DIR=%BIN_OUTPUT_DIR%\helpers
if not exist "%HELPERS_OUTPUT_DIR%" mkdir %HELPERS_OUTPUT_DIR%
call :copybinary fastarp %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
call :copybinary fastping %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
call :copybinary ipreader %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%

:dreamsdk_ide_patchers
call :log Copying IDE Patchers...
set CODEBLOCKS_PATCHER_OUTPUT_DIR=%BIN_OUTPUT_DIR%\packages\ide\codeblocks
if not exist "%CODEBLOCKS_PATCHER_OUTPUT_DIR%" mkdir %CODEBLOCKS_PATCHER_OUTPUT_DIR%
call :copybinary codeblocks-patcher %CODEBLOCKS_PATCHER_INPUT_DIR% %CODEBLOCKS_PATCHER_OUTPUT_DIR%

:dreamsdk_binaries
call :log Copying Binaries...
call :copybinary dreamsdk-manager %MANAGER_INPUT_DIR% %BIN_OUTPUT_DIR%
call :copybinary dreamsdk-shell %SHELL_INPUT_DIR% %BIN_OUTPUT_DIR%
call :copybinary dreamsdk-runner %RUNNER_INPUT_DIR% %BIN_OUTPUT_DIR%

call :log
call :log Done!

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
call :err Please check all the input directories.
goto end

:err_output_dir
call :err The specified output directory (OUTPUT_DIR) was not found.
call :log Directory: "%OUTPUT_DIR%"
goto end

:err_dreamsdk_missing
call :err Please install DreamSDK before using this script.
goto end

:err_offline
call :err Missing embedded/offline files. Please run the `Offline` script.	
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
setlocal EnableDelayedExpansion
set _extract_to_parent=0
set _installable_package=%1
if "+%_installable_package%"=="+%INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT%" (
  set _installable_package=1
  set _extract_to_parent=1
)
if "+%_installable_package%"=="+%NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT%" (
  set _installable_package=0
  set _extract_to_parent=1
)
set _base=%2
set _name=%2
set _ver=%3
if not "_%4"=="_" (
  set _base=%4\%_base%
  set _name=%2-%4
)
set _input=%SETUP_PACKAGES_INPUT_DIR%\%_base%\%3\%2-bin.7z
if not exist %_input% set _input=%SETUP_PACKAGES_INPUT_DIR%\%_base%\%3\%4-%2-bin.7z
set _output=%OUTPUT_DIR%\%_base%
if not exist %_input% (
  set _input=%SETUP_PACKAGES_INPUT_DIR%\%2\%3\%2-%4-bin.7z
  set _output=%OUTPUT_DIR%\%2\%2-%4
  set _name=%2-%4
)
if not "$%4"=="$" (
  if not "#%5"=="#" (
    set _input=%SETUP_PACKAGES_INPUT_DIR%\%_base%\%3\%2-%5-bin.7z
    set _output=%OUTPUT_DIR%\%4\%2-%5
    set _name=%2-%4::%5
  )
)
if "$%_extract_to_parent%"=="$1" (
  set _output=%_output%\..
)
if exist %_input% (
  call :log * Unpacking %_name% ^(%_ver%^) ...  
  %SEVENZIP% x "%_input%" -o"%_output%" -y >> %LOG_FILE% 2>&1
  if "$%_installable_package%"=="$1" (    
    copy /B %_input% %BIN_PACKAGES_OUTPUT_DIR% >> %LOG_FILE% 2>&1
  )
)
endlocal
goto :EOF

:copybinary
setlocal EnableDelayedExpansion
set _name=%1
set _src=%2
set _target=%3
call :log * Copying Binary: %_name% ...
set _binary=%_src%\%_name%\bin\%_name%.exe
if not exist "%_binary%" set _binary=%_src%\bin\%_name%.exe
if not exist "%_binary%" (
  call :err ** Missing Binary: %_name%
  call :log ** Please build it in RELEASE mode using Lazarus IDE.
  goto end
)
copy /B %_binary% %_target% >> %LOG_FILE% 2>&1
endlocal
goto :EOF

:win2unix
setlocal EnableDelayedExpansion
call :win2unixsub %%%1%%
set tmpwin2unix=%tmpwin2unix:\=/%
set tmpwin2unix=/%tmpwin2unix::=%
endlocal & set %1=%tmpwin2unix%
goto :EOF
:win2unixsub
set tmpwin2unix=%*
goto :EOF

:movedir
setlocal EnableDelayedExpansion
set _source=%1
set _target=%2
robocopy %_source% %_target% /s /move >> %LOG_FILE% 2>&1
::call :win2unix _source
::call :win2unix _target
::set MOVEDIR_BASH_SCRIPT=%BASE_DIR%\data\move-dir.sh
::call :win2unix MOVEDIR_BASH_SCRIPT
::%RUNNER% %MOVEDIR_BASH_SCRIPT% "%_source%" "%_target%" >> %LOG_FILE% 2>&1
endlocal
goto :EOF
