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

:init
rem Global boolean variable used in various locations
set FUNC_RESULT=0

rem Check if DreamSDK is installed (of course, you can use a previous version!)
if "$%DREAMSDK_HOME%"=="$" goto err_dreamsdk_missing

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\prepare.ini
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
set RELMODE="%PYTHON%" "%BASE_DIR%\data\relmode.py"

rem Input directories
call :checkdir FUNC_RESULT %CODEBLOCKS_PATCHER_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :checkdir FUNC_RESULT %DOCUMENTATION_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :checkdir FUNC_RESULT %HELPERS_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :checkdir FUNC_RESULT %MANAGER_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :checkdir FUNC_RESULT %RUNNER_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :checkdir FUNC_RESULT %SETUP_PACKAGES_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :checkdir FUNC_RESULT %SHELL_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :checkdir FUNC_RESULT %SYSTEM_OBJECTS_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

rem Output directory
call :checkdir FUNC_RESULT %SETUP_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

rem Handling directory: .sources
set OUTPUT_DIR=%SETUP_OUTPUT_DIR%\.sources
call :checkdir FUNC_RESULT %OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

rem Handling directory: dreamsdk-binaries 
set BIN_OUTPUT_DIR=%OUTPUT_DIR%\dreamsdk-binaries
call :checkdir FUNC_RESULT %BIN_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

rem Handling directory: binary-packages
set BIN_PACKAGES_OUTPUT_DIR=%OUTPUT_DIR%\binary-packages
call :checkdir FUNC_RESULT %BIN_PACKAGES_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

:check_sevenzip
call :checkfile FUNC_RESULT %SEVENZIP%
if "+%FUNC_RESULT%"=="+0" goto err_binary_sevenzip

:check_upx
call :checkfile FUNC_RESULT %UPX32%
if "+%FUNC_RESULT%"=="+0" goto err_binary_upx

:check_python
call :checkfile FUNC_RESULT %PYTHON%
if "+%FUNC_RESULT%"=="+0" goto err_binary_python
set PYTHON_VERSION_MAJOR=
set PYTHON_VERSION=
call :get_version_python PYTHON_VERSION_MAJOR PYTHON_VERSION
if "$%PYTHON_VERSION_MAJOR%"=="$3" goto start
goto err_binary_python

:start
pushd .

:lib_embedded
call :log Checking embedded libraries...
if not exist "%OUTPUT_DIR%\lib-embedded" goto err_offline

:source_packages
call :log Checking embedded source packages...
if not exist "%OUTPUT_DIR%\source-packages" goto err_offline

:system_objects
call :log Generating system objects...

call :copy "%SYSTEM_OBJECTS_INPUT_DIR%\mingw" "%OUTPUT_DIR%\system-objects"
set SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR=%OUTPUT_DIR%\system-objects-configuration
if not exist %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR% mkdir %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR%

:setup_helpers
call :log Copying Setup Helpers...
set SETUP_HELPERS_OUTPUT_DIR=%SETUP_OUTPUT_DIR%\.helpers
if not exist %SETUP_HELPERS_OUTPUT_DIR% mkdir %SETUP_HELPERS_OUTPUT_DIR%

call :copybinary FUNC_RESULT cbhelper %BASE_DIR%\..\embedded %SETUP_HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT pecheck %BASE_DIR%\..\embedded %SETUP_HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_helpers
call :log Copying Helpers...
set HELPERS_OUTPUT_DIR=%BIN_OUTPUT_DIR%\helpers
if not exist %HELPERS_OUTPUT_DIR% mkdir %HELPERS_OUTPUT_DIR%

call :copybinary FUNC_RESULT fastarp %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT fastping %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT ipreader %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_ide_patchers
call :log Copying IDE Patchers...
set CODEBLOCKS_PATCHER_OUTPUT_DIR=%BIN_OUTPUT_DIR%\packages\ide\codeblocks
if not exist %CODEBLOCKS_PATCHER_OUTPUT_DIR% mkdir %CODEBLOCKS_PATCHER_OUTPUT_DIR%

call :copybinary FUNC_RESULT codeblocks-patcher %CODEBLOCKS_PATCHER_INPUT_DIR% %CODEBLOCKS_PATCHER_OUTPUT_DIR% "--compress-resources=0"
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_binaries
call :log Copying Binaries...

call :copybinary FUNC_RESULT dreamsdk-manager %MANAGER_INPUT_DIR% %BIN_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT dreamsdk-shell %SHELL_INPUT_DIR% %BIN_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT dreamsdk-runner %RUNNER_INPUT_DIR% %BIN_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_help
call :log Copying Help...
set HELP_INPUT_FILE=%DOCUMENTATION_INPUT_DIR%\bin\dreamsdk.chm
if not exist %HELP_INPUT_FILE% goto err_help_missing
copy /B %HELP_INPUT_FILE% %BIN_OUTPUT_DIR% >> %LOG_FILE% 2>&1

:extract
call :log Extracting packages...

set NOT_INSTALLABLE_PACKAGE=0
set INSTALLABLE_PACKAGE=1
set INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT=2
set NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT=3
set INSTALLABLE_PACKAGE_OPTIONAL=4

rem Extracting MinGW foundation base package
call :unpack %NOT_INSTALLABLE_PACKAGE% mingw-base %MINGW_BASE_VERSION%

rem Extracting MSYS packages
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% cdrtools %MSYS_BASE_CDRTOOLS_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% curl %MSYS_BASE_CURL_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% dirhash %MSYS_BASE_DIRHASH_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% gawk %MSYS_BASE_GAWK_VERSION% msys-base
call :unpack %NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT% img4dc %MSYS_BASE_IMG4DC_VERSION% msys-base
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
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% no-python
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-2.7
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.0
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.1
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.2
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.3
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.4
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.5
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.6
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.7
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.8
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.9
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.10
call :unpack %INSTALLABLE_PACKAGE_OPTIONAL% sh-elf-gdb %SH_ELF_GDB_VERSION% python-3.11

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

:finish
call :log
call :log Done!
call :log

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
call :err Please check all input directories.
goto end

:err_output_dir
call :err The specified output directory (SETUP_OUTPUT_DIR) was not found.
call :log Directory: "%SETUP_OUTPUT_DIR%"
goto end

:err_dreamsdk_missing
call :err Please install DreamSDK before using this script.
goto end

:err_binary_sevenzip
call :err 7-Zip was not found.
call :log File: "%SEVENZIP%"
goto end

:err_binary_python
call :err Python 3 was not found.
call :log File: "%PYTHON%"
goto end

:err_binary_upx
call :err UPX 32-bit was not found.
call :log File: "%UPX32%"
goto end

:err_offline
call :err Missing embedded/offline files. Please run the "Offline" script.
call :log This script is located in "setup-helpers" as well.
goto end

:err_help_missing
call :err Missing Help CHM file. Please build it using "KEL CHM Creator".
call :log Please read the "documentation" repository for more information.
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

:unpack
setlocal EnableDelayedExpansion
set _installable_package=%1
set _extract_to_parent=0
set _warn_if_package_not_found=1
if "+%_installable_package%"=="+%INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT%" (
  set _installable_package=1
  set _extract_to_parent=1
)
if "+%_installable_package%"=="+%NOT_INSTALLABLE_PACKAGE_EXTRACT_TO_PARENT%" (
  set _installable_package=0
  set _extract_to_parent=1
)
if "%_installable_package%"=="%INSTALLABLE_PACKAGE_OPTIONAL%" (
  set _installable_package=1
  set _warn_if_package_not_found=0
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
if "%_warn_if_package_not_found%"=="0" goto unpack_exit
if not exist %_input% (
  call :warn Package not found: %_name% ^(%_ver%^)
)
:unpack_exit
endlocal
goto :EOF

:copybinary
setlocal EnableDelayedExpansion
set _result=1
set _name=%2
set _src=%3
set _target=%4
set _upx_optional_switches=%5
call :log * Copying Binary: %_name% ...
set _binary=%_src%\%_name%\bin\%_name%.exe
if not exist "%_binary%" set _binary=%_src%\bin\%_name%.exe
if not exist "%_binary%" (
  call :err Missing Binary: "%_name%".
  call :log Please build it in RELEASE mode using Lazarus IDE.
  set _result=0
  goto copybinaryexit
)
copy /B %_binary% %_target% >> %LOG_FILE% 2>&1
:copybinarycheck
set _mode=
set _tmpfile=%_name%.tmp
%RELMODE% %_binary% > %_tmpfile%
if exist "%_tmpfile%" (
  set /p _mode=<%_tmpfile%
  del %_tmpfile%
)
if "+%_mode%"=="+DEBUG" goto copybinarycheckdebug
if "+%_mode%"=="+RELEASE" goto copybinarycompress
call :warn Undefined mode for %_name%: %_mode%
goto copybinarycompress
:copybinarycheckdebug
call :warn %_name% is compiled in DEBUG mode...
:copybinarycompress
%UPX32% -9 %_upx_optional_switches% %_target%\%_name%.exe >> %LOG_FILE% 2>&1
:copybinaryexit
endlocal & (
	set "%~1=%_result%"	
)
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
