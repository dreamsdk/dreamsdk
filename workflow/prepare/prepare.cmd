@echo off
set APP_TITLE=Source Packages Preparer for DreamSDK Setup
title %APP_TITLE%
cls

rem Debug Mode: Enable me if you want to troubleshoot something
set DEBUG_MODE=0

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%
set SETUP_PACKAGES_INPUT_DIR=%BASE_DIR%\pkgcache

set LOG_FILE=%BASE_DIR%\prepare.log
if exist %LOG_FILE% del %LOG_FILE%

:banner
call :log =============================================================================
call :log DreamSDK - %APP_TITLE%
call :log =============================================================================
call :log

:init
rem Global boolean variable used in various locations
set FUNC_RESULT=0

rem Check if DreamSDK is installed (of course, you can use a previous version!)
if "$%DREAMSDK_HOME%"=="$" goto err_dreamsdk_missing

rem Read General Configuration
set "CONFIG_FILE=%BASE_DIR%\prepare.ini"
if not exist "%CONFIG_FILE%" set "CONFIG_FILE=%BASE_DIR%\prepare.default.ini"
if not exist "%CONFIG_FILE%" goto err_config
for /f "tokens=*" %%i in (%CONFIG_FILE%) do (
  set %%i 2> nul
  rem Sanitize configuration entry
  for /f "tokens=1 delims==" %%j in ("%%i") do (
    call :trim %%j
  )
)

rem Read Packages Configuration
set "PACKAGES_CONFIG_FILE=%BASE_DIR%\packages.ini"
if not exist "%PACKAGES_CONFIG_FILE%" set "PACKAGES_CONFIG_FILE=%BASE_DIR%\packages.default.ini"
if not exist "%PACKAGES_CONFIG_FILE%" goto err_config_packages
for /f "tokens=*" %%i in (%PACKAGES_CONFIG_FILE%) do (
  set %%i 2> nul
  rem Sanitize configuration entry
  for /f "tokens=1 delims==" %%j in ("%%i") do (
    call :trim %%j
  )
)

rem Just display the final output directory read from config file...
call :log Target output directory: "%SETUP_OUTPUT_DIR%"
call :log

rem Utilities
set PATCH="%DREAMSDK_HOME%\msys\1.0\bin\patch.exe"
if not exist %PATCH% set PATCH="%DREAMSDK_HOME%\usr\bin\patch.exe"
set RELMODE="%PYTHON%" "%BASE_DIR%\data\relmode.py"
set MKCFGGDB="%PYTHON%" "%BASE_DIR%\data\mkcfggdb.py"
set MKCFGTOOLCHAINS="%PYTHON%" "%BASE_DIR%\data\mkcfgtoolchains.py"
set DUALSIGN="%SETUP_OUTPUT_DIR%\tools\dualsign\dualsign.cmd"
set WGET="%DREAMSDK_HOME%\msys\1.0\bin\wget.exe"
if not exist %WGET% set WGET="%DREAMSDK_HOME%\usr\bin\wget.exe"

rem Input directories
call :normalizepath CODEBLOCKS_PATCHER_INPUT_DIR
call :checkdir FUNC_RESULT %CODEBLOCKS_PATCHER_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :normalizepath DOCUMENTATION_INPUT_DIR
call :checkdir FUNC_RESULT %DOCUMENTATION_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :normalizepath HELPERS_INPUT_DIR
call :checkdir FUNC_RESULT %HELPERS_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :normalizepath MANAGER_INPUT_DIR
call :checkdir FUNC_RESULT %MANAGER_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :normalizepath RUNNER_INPUT_DIR
call :checkdir FUNC_RESULT %RUNNER_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :normalizepath SHELL_INPUT_DIR
call :checkdir FUNC_RESULT %SHELL_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

call :normalizepath SYSTEM_OBJECTS_INPUT_DIR
call :checkdir FUNC_RESULT %SYSTEM_OBJECTS_INPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_input_dir

rem Output directory
call :normalizepath SETUP_OUTPUT_DIR
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

set BIN64_OUTPUT_DIR=%OUTPUT_DIR%\dreamsdk-binaries-x64
call :checkdir FUNC_RESULT %BIN64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

rem Handling directory: binary-packages
set BIN_PACKAGES_OUTPUT_DIR=%OUTPUT_DIR%\binary-packages
call :checkdir FUNC_RESULT %BIN_PACKAGES_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

set BIN64_PACKAGES_OUTPUT_DIR=%OUTPUT_DIR%\binary-packages-x64
call :checkdir FUNC_RESULT %BIN64_PACKAGES_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

set SETUP_CONFIG_OUTPUT_DIR=%SETUP_OUTPUT_DIR%\src\cfg
call :checkdir FUNC_RESULT %SETUP_CONFIG_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto err_output_dir

:check_sevenzip
call :checkfile FUNC_RESULT %SEVENZIP%
if "+%FUNC_RESULT%"=="+0" goto err_binary_sevenzip

:check_upx
call :checkfile FUNC_RESULT %UPXPACK%
if "+%FUNC_RESULT%"=="+0" goto err_binary_upx

:check_hhc
call :checkfile FUNC_RESULT %HHC%
if "+%FUNC_RESULT%"=="+0" goto err_hhc_missing

:check_python
call :checkfile FUNC_RESULT %PYTHON%
if "+%FUNC_RESULT%"=="+0" goto err_binary_python
set PYTHON_VERSION_MAJOR=
set PYTHON_VERSION=
call :get_version_python PYTHON_VERSION_MAJOR PYTHON_VERSION
if "$%PYTHON_VERSION_MAJOR%"=="$3" goto check_dualsign
goto err_binary_python

:check_dualsign
if "%SIGN_BINARIES%+"=="1+" (
  if not exist %DUALSIGN% goto err_binary_dualsign
)

:start
pushd .

:lib_embedded
call :log Checking embedded libraries...
if not exist "%OUTPUT_DIR%\lib-embedded" goto err_offline

:source_packages
call :log Checking embedded source packages...
if not exist "%OUTPUT_DIR%\source-packages" goto err_offline

:system_objects
call :log Generating objects...

call :copy "%SYSTEM_OBJECTS_INPUT_DIR%\files\mingw" "%OUTPUT_DIR%\msys-system-objects"
call :copy "%SYSTEM_OBJECTS_INPUT_DIR%\files\mingw64" "%OUTPUT_DIR%\msys2-system-objects"
call :copy "%SYSTEM_OBJECTS_INPUT_DIR%\files\common" "%OUTPUT_DIR%\dreamsdk-objects"
set SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR=%OUTPUT_DIR%\msys-system-objects-configuration
if not exist %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR% mkdir %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR%

:setup_helpers
call :log Copying Setup Helpers...
set SETUP_HELPERS_OUTPUT_DIR=%SETUP_OUTPUT_DIR%\.helpers
if not exist %SETUP_HELPERS_OUTPUT_DIR% mkdir %SETUP_HELPERS_OUTPUT_DIR%

call :copybinary FUNC_RESULT common 32 %SETUP_HELPERS_INPUT_DIR% %SETUP_HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT cbhelper 32 %SETUP_HELPERS_INPUT_DIR% %SETUP_HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_helpers
call :log Copying Helpers...
set HELPERS_OUTPUT_DIR=%BIN_OUTPUT_DIR%\helpers
if not exist %HELPERS_OUTPUT_DIR% mkdir %HELPERS_OUTPUT_DIR%

call :copybinary FUNC_RESULT fastarp 32 %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT fastping 32 %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT ipreader 32 %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT kosports 32 %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT mkdirln 32 %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT wtconfig 32 %HELPERS_INPUT_DIR% %HELPERS_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_ide_patchers
call :log Copying IDE Patchers...
set CODEBLOCKS_PATCHER_OUTPUT_DIR=%BIN_OUTPUT_DIR%\packages\ide\codeblocks
if not exist %CODEBLOCKS_PATCHER_OUTPUT_DIR% mkdir %CODEBLOCKS_PATCHER_OUTPUT_DIR%

call :copybinary FUNC_RESULT codeblocks-patcher 32 %CODEBLOCKS_PATCHER_INPUT_DIR% %CODEBLOCKS_PATCHER_OUTPUT_DIR% "--compress-resources=0"
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_binaries
call :log Copying Binaries...

call :copybinary FUNC_RESULT dreamsdk-manager 32 %MANAGER_INPUT_DIR% %BIN_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT dreamsdk-shell 32 %SHELL_INPUT_DIR% %BIN_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT dreamsdk-runner 32 %RUNNER_INPUT_DIR% %BIN_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_help
call :log Copying Help...
set HELP_INPUT_FILE=%DOCUMENTATION_INPUT_DIR%\bin\dreamsdk.chm
%HHC% "%DOCUMENTATION_INPUT_DIR%\src\dreamsdk.hhp" >> %LOG_FILE% 2>&1
copy /B %HELP_INPUT_FILE% %BIN_OUTPUT_DIR% >> %LOG_FILE% 2>&1

:dreamsdk_helpers64
call :log Copying Helpers (x64)...
set HELPERS64_OUTPUT_DIR=%BIN64_OUTPUT_DIR%\helpers
if not exist %HELPERS64_OUTPUT_DIR% mkdir %HELPERS64_OUTPUT_DIR%

call :copybinary FUNC_RESULT fastarp 64 %HELPERS_INPUT_DIR% %HELPERS64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT fastping 64 %HELPERS_INPUT_DIR% %HELPERS64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT ipreader 64 %HELPERS_INPUT_DIR% %HELPERS64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT kosports 64 %HELPERS_INPUT_DIR% %HELPERS64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT mkdirln 64 %HELPERS_INPUT_DIR% %HELPERS64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT wtconfig 64 %HELPERS_INPUT_DIR% %HELPERS64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_ide_patchers64
call :log Copying IDE Patchers (x64)...
set CODEBLOCKS_PATCHER64_OUTPUT_DIR=%BIN64_OUTPUT_DIR%\packages\ide\codeblocks
if not exist %CODEBLOCKS_PATCHER64_OUTPUT_DIR% mkdir %CODEBLOCKS_PATCHER64_OUTPUT_DIR%

call :copybinary FUNC_RESULT codeblocks-patcher 64 %CODEBLOCKS_PATCHER_INPUT_DIR% %CODEBLOCKS_PATCHER64_OUTPUT_DIR% "--compress-resources=0"
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_binaries64
call :log Copying Binaries (x64)...

call :copybinary FUNC_RESULT dreamsdk-manager 64 %MANAGER_INPUT_DIR% %BIN64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT dreamsdk-shell 64 %SHELL_INPUT_DIR% %BIN64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

call :copybinary FUNC_RESULT dreamsdk-runner 64 %RUNNER_INPUT_DIR% %BIN64_OUTPUT_DIR%
if "+%FUNC_RESULT%"=="+0" goto end

:dreamsdk_help64
call :log Copying Help (x64)...
set HELP_INPUT_FILE=%DOCUMENTATION_INPUT_DIR%\bin\dreamsdk.chm
%HHC% "%DOCUMENTATION_INPUT_DIR%\src\dreamsdk.hhp" >> %LOG_FILE% 2>&1
copy /B %HELP_INPUT_FILE% %BIN64_OUTPUT_DIR% >> %LOG_FILE% 2>&1

:processing
call :log Processing packages ...

rem Downloading all packages in a local cache
call :buildpkgcache

set PROCESSPKG_UNPACK=0
set PROCESSPKG_UNPACK_EXTRACT_TO_PARENT=1
set PROCESSPKG_TOOLCHAIN=2
set PROCESSPKG_TOOLCHAIN_OPTIONAL=3
set PROCESSPKG_TOOLCHAIN64=4
set PROCESSPKG_TOOLCHAIN_OPTIONAL64=5

:processing_base
call :log Processing foundation base packages ...
rem MinGW x86
call :processpkg %PROCESSPKG_UNPACK% mingw-base %MINGW32_BASE_VERSION%
rem MinGW x64
call :processpkg %PROCESSPKG_UNPACK% mingw64-base %MINGW64_BASE_VERSION%

:processing_base_mingw_profile
call :log * Generating profile file for mingw-base ...
set SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR=%SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR%\etc
set SYSTEM_OBJECTS_PROFILE_INPUT_FILE=%OUTPUT_DIR%\mingw-base\msys\1.0\etc\profile
set SYSTEM_OBJECTS_PROFILE_OUTPUT_FILE=%SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR%\profile
if not exist %SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR% mkdir %SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR%
if exist %SYSTEM_OBJECTS_PROFILE_OUTPUT_FILE% del %SYSTEM_OBJECTS_PROFILE_OUTPUT_FILE%
if not exist %SYSTEM_OBJECTS_PROFILE_INPUT_FILE% (
    call :err File not found: "%SYSTEM_OBJECTS_PROFILE_INPUT_FILE%"
    goto end
)
move %SYSTEM_OBJECTS_PROFILE_INPUT_FILE% %SYSTEM_OBJECTS_CONFIGURATION_ETC_OUTPUT_DIR% >> %LOG_FILE% 2>&1
call :patch FUNC_RESULT %SYSTEM_OBJECTS_CONFIGURATION_OUTPUT_DIR% %SYSTEM_OBJECTS_INPUT_DIR%\patches\etc.diff
if "+%FUNC_RESULT%"=="+0" goto end

:processing_msys
call :log Processing MSYS packages ...
rem MSYS x86
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% bash %MSYS32_BASE_BASH_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% curl %MSYS32_BASE_CURL_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% dirhash %MSYS32_BASE_DIRHASH_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% gawk %MSYS32_BASE_GAWK_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% libelf %MSYS32_BASE_LIBELF_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% libjpeg %MSYS32_BASE_LIBJPEG_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% libpng %MSYS32_BASE_LIBPNG_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% mintty %MSYS32_BASE_MINTTY_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% msys-core-extended %MSYS32_BASE_CORE_EXTENDED_VERSION% msys-base
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% wget %MSYS32_BASE_WGET_VERSION% msys-base
rem MSYS x64
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% dirhash %MSYS64_BASE_DIRHASH_VERSION% msys2-base

:processing_utilities
call :log Processing utilities packages ...
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% cdrtools %UTILITIES_CDRTOOLS_VERSION% utilities
call :processpkg %PROCESSPKG_UNPACK_EXTRACT_TO_PARENT% img4dc %UTILITIES_IMG4DC_VERSION% utilities

:processing_addons
call :log Processing addons command-line tools packages ...
call :processpkg %PROCESSPKG_UNPACK% elevate %ADDONS_CMD_ELEVATE_VERSION% addons-cmd
call :processpkg %PROCESSPKG_UNPACK% pvr2png %ADDONS_CMD_PVR2PNG_VERSION% addons-cmd
call :processpkg %PROCESSPKG_UNPACK% txfutils %ADDONS_CMD_TXFUTILS_VERSION% addons-cmd
call :processpkg %PROCESSPKG_UNPACK% txfutils %ADDONS_CMD_TXFUTILS_VERSION% addons-cmd txflib
call :processpkg %PROCESSPKG_UNPACK% vmutool %ADDONS_CMD_VMUTOOL_VERSION% addons-cmd

call :log Processing addons GUI tools packages ...
call :processpkg %PROCESSPKG_UNPACK% bdreams %ADDONS_GUI_BDREAMS_VERSION% addons-gui
call :processpkg %PROCESSPKG_UNPACK% buildsbi %ADDONS_GUI_BUILDSBI_VERSION% addons-gui
call :processpkg %PROCESSPKG_UNPACK% checker %ADDONS_GUI_CHECKER_VERSION% addons-gui
call :processpkg %PROCESSPKG_UNPACK% ipwriter %ADDONS_GUI_IPWRITER_VERSION% addons-gui
call :processpkg %PROCESSPKG_UNPACK% ipwriter %ADDONS_GUI_IPWRITER_VERSION% addons-gui iplogos
call :processpkg %PROCESSPKG_UNPACK% mrwriter %ADDONS_GUI_MRWRITER_VERSION% addons-gui
call :processpkg %PROCESSPKG_UNPACK% sbinducr %ADDONS_GUI_SBINDUCR_VERSION% addons-gui
call :processpkg %PROCESSPKG_UNPACK% vmutool %ADDONS_GUI_VMUTOOL_VERSION% addons-gui

:processing_toolchains
call :log Processing toolchains packages ...
call :processtoolchains 32 "%TOOLCHAINS32_VERSIONS%"

call :log Processing toolchains packages (x64)...
call :processtoolchains 64 "%TOOLCHAINS64_VERSIONS%"

:processing_gdb
call :log Processing GDB: GNU Debugger packages ...
call :processgdb 32 %GDB32_VERSION%

call :log Processing GDB: GNU Debugger packages (x64)...
call :processgdb 64 %GDB64_VERSION%

:processing_inno_setup
call :log Generating Inno Setup configuration files ...

call :log * Generating GDB configuration file...
%MKCFGGDB% %SETUP_CONFIG_OUTPUT_DIR% %GDB32_VERSION% %GDB64_VERSION% %BIN_PACKAGES_OUTPUT_DIR% %BIN64_PACKAGES_OUTPUT_DIR% >> %LOG_FILE% 2>&1

call :log * Generating toolchains configuration file...
%MKCFGTOOLCHAINS% %SETUP_CONFIG_OUTPUT_DIR% %PACKAGES_CONFIG_FILE% >> %LOG_FILE% 2>&1

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

:err_config_packages
call :err The packages file was not found.
call :log File: "%PACKAGES_CONFIG_FILE%"
goto end

:err_input_dir
call :err Please check all input directories.
goto end

:err_output_dir
call :err The specified output directory (SETUP_OUTPUT_DIR) was not found.
call :log Directory: "%SETUP_OUTPUT_DIR%"
goto end

:err_dreamsdk_missing
call :err Please install a previous version of DreamSDK before using this script.
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
call :err UPX was not found.
call :log File: "%UPXPACK%"
goto end

:err_binary_dualsign
call :err DualSign utility was not found.
call :log File: "%DUALSIGN%"
goto end

:err_offline
call :err Missing embedded/offline files. Please run the Offline script first.
goto end

:err_hhc_missing
call :err Missing Microsoft HTML Help Workshop. Please install it.
call :log Please read the "documentation" repository for more information.
goto end

rem ## Utilities ###############################################################

:normalizepath
rem Thanks to: https://stackoverflow.com/a/33404867
setlocal EnableDelayedExpansion
call :normalizepathsub %%%1%%
endlocal & (
	set "%1=%_normalizepathsub_absolutepath%"
)
goto :EOF
:normalizepathsub
set _normalizepathsub_absolutepath=%~f1
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

:tolower
setlocal EnableDelayedExpansion
set "_varname=%~1"
set "_str=!%_varname%!"
for /f "delims=" %%a in ('%PYTHON% -c "import sys; print(sys.argv[1].lower())" "!_str!"') do (
  set "_result=%%a"
)
endlocal & (
  set "%_varname%=%_result%"
)
goto :EOF

:copy
set EXCLUDE_FILE=%BASE_DIR%\exclude.txt
echo .git\ > %EXCLUDE_FILE%
echo .svn\ >> %EXCLUDE_FILE%
xcopy %1\* %2 /exclude:%EXCLUDE_FILE% /s /i /y >> %LOG_FILE% 2>&1
if exist %EXCLUDE_FILE% del %EXCLUDE_FILE%
goto :EOF

:patch
setlocal EnableDelayedExpansion
set _result=1
set _target=%2
set _patch=%3
%PATCH% -N -d %_target% -p1 -r - < %_patch% >> %LOG_FILE% 2>&1
if "!errorlevel!+"=="0+" goto patchexit
call :err Failed Patch: "%_patch%".
set _result=0
:patchexit
endlocal & (
    set "%~1=%_result%"
)
goto :EOF

:warn
call :log WARNING: %*
goto :EOF

:err
call :log ERROR: %*
goto :EOF

:log
setlocal EnableDelayedExpansion
set "tmplog=%*"
if "!tmplog!"=="" goto log_empty
echo !tmplog!
echo !tmplog! >> %LOG_FILE% 2>&1
endlocal
goto :EOF
:log_empty
echo.
echo. >> %LOG_FILE% 2>&1
goto :EOF

:processpkg
setlocal EnableDelayedExpansion
rem Get all parameters from this function
set _behaviour=%1
set _pkgname=%2
set _pkgver=%3
set _pkgvariant=%4
set _pkgextra=%5
set _pkgextra2=%6
rem Translate x64 values to real process values
set _pkg64=0
if "+%_behaviour%"=="+%PROCESSPKG_TOOLCHAIN64%" (
  set "_behaviour=%PROCESSPKG_TOOLCHAIN%"
  set "_pkg64=1"
)
if "+%_behaviour%"=="+%PROCESSPKG_TOOLCHAIN_OPTIONAL64%" (
  set "_behaviour=%PROCESSPKG_TOOLCHAIN_OPTIONAL%"
  set "_pkg64=1"
)
rem Set flags to their default values
set _unpack_required=0
set _pkgextract_to_parent=0
set _warn_if_package_not_found=1
rem Apply the correct flag configuration depending on behaviour
if "+%_behaviour%"=="+%PROCESSPKG_UNPACK%" (
  set _unpack_required=1
)
if "+%_behaviour%"=="+%PROCESSPKG_UNPACK_EXTRACT_TO_PARENT%" (
  set _unpack_required=1
  set _pkgextract_to_parent=1
)
if "+%_behaviour%"=="+%PROCESSPKG_TOOLCHAIN_OPTIONAL%" (
  set _warn_if_package_not_found=0
)
rem Debug: Display variables
if "+%DEBUG_MODE%"=="+1" (
  echo * Variables:
  echo _behaviour=%_behaviour%
  echo _pkgname=%_pkgname%
  echo _pkgver=%_pkgver%
  echo _pkgvariant=%_pkgvariant%
  echo _pkgextra=%_pkgextra%
  echo * Flags:
  echo _unpack_required=%_unpack_required%
  echo _pkgextract_to_parent=%_pkgextract_to_parent%
  echo _warn_if_package_not_found=%_warn_if_package_not_found%
  echo _pkg64=%_pkg64%
)
rem Handle x64 variant
set "_pkgsuffix64="
if "+%_pkg64%"=="+1" set "_pkgsuffix64=-x64"
rem Compute input/output variables
set "_binpackageoutputdir=%BIN_PACKAGES_OUTPUT_DIR%"
if "+%_pkg64%"=="+1" set "_binpackageoutputdir=%BIN64_PACKAGES_OUTPUT_DIR%"
set _pkgdisplayname=%_pkgname%
set _pkgbasefilename=%_pkgname%-bin
set _pkgtempbase=%_pkgname%
if not "+%_pkgvariant%"=="+" (
  set _pkgtempbase=%_pkgvariant%\%_pkgtempbase%
  set _pkgdisplayname=%_pkgname%-%_pkgvariant%
)
set _output=%OUTPUT_DIR%\%_pkgtempbase%
if not "+%_pkgextra%"=="+" (
  set _pkgdisplayname=%_pkgdisplayname%::%_pkgextra%
  set _pkgbasefilename=%_pkgname%-%_pkgextra%-bin
  set _output=%OUTPUT_DIR%\%_pkgvariant%%_pkgsuffix64%\%_pkgname%-%_pkgextra%
)
set _input=%SETUP_PACKAGES_INPUT_DIR%\%_pkgtempbase%\%_pkgver%\%_pkgbasefilename%.7z
if not exist %_input% (
  set _pkgbasefilename=%_pkgvariant%-%_pkgbasefilename%
  set _input=%SETUP_PACKAGES_INPUT_DIR%\%_pkgtempbase%\%_pkgver%\%_pkgbasefilename%.7z
)
if not exist %_input% (
  set _pkgdisplayname=%_pkgname%-%_pkgvariant%
  set _pkgbasefilename=%_pkgname%-%_pkgvariant%-bin
  set _input=%SETUP_PACKAGES_INPUT_DIR%\%_pkgname%\%_pkgver%\!_pkgbasefilename!.7z
  set _output=%OUTPUT_DIR%\%_pkgname%%_pkgsuffix64%\%_pkgname%-%_pkgvariant%
)
if "+%_behaviour%"=="+%PROCESSPKG_TOOLCHAIN%" (
  set _pkgdisplayname=%_pkgname%-%_pkgvariant%
  set _pkgbasefilename=!_pkgname!-bin
  set _input=%SETUP_PACKAGES_INPUT_DIR%\%_pkgname%\%_pkgver%\!_pkgbasefilename!.7z
  set _output=%OUTPUT_DIR%\%_pkgextra%-%_pkgvariant%%_pkgsuffix64%\%_pkgextra2%
)
set _pkginputfilename=%_pkgbasefilename%
for %%f in ("%_input%") do set _pkginputfilename=%%~nf
if "$%_pkgextract_to_parent%"=="$1" (
  set _output=%_output%\..
)
set _stampfilepath=.
if "+%_behaviour%"=="+%PROCESSPKG_TOOLCHAIN_OPTIONAL%" (
  set _stampfilepath=%_output%\%_pkginputfilename%.stamp
)
if "+%_behaviour%"=="+%PROCESSPKG_TOOLCHAIN%" (
  set _stampfilepath=%_output%\%_pkgname%-%_pkgvariant%-bin.stamp
)
if "+%_behaviour%"=="+%PROCESSPKG_UNPACK_EXTRACT_TO_PARENT%" (
  set _pkgdisplayname=%_pkgname%
)
if "+%_behaviour%"=="+%PROCESSPKG_UNPACK%" (
  set _pkgdisplayname=%_pkgname%
)
rem Debug: Display variables for targets
if "+%DEBUG_MODE%"=="+1" (
  echo * Targets:
  echo _input=%_input%
  echo _output=%_output%
  echo _pkgdisplayname=%_pkgdisplayname%
  echo _pkginputfilename=%_pkginputfilename%
  pause
)
rem Execute the processpkg operation
if exist %_input% (
  if "+%_unpack_required%"=="+1" (
    call :log * Unpacking %_pkgdisplayname% ^(%_pkgver%^) ...  
    %SEVENZIP% x "%_input%" -o"%_output%" -y >> %LOG_FILE% 2>&1
  ) else (
    rem Copy the package to the "binary-packages" directory
    call :log * Copying %_pkgdisplayname% ^(%_pkgver%^) ...
    copy /B %_input% %_binpackageoutputdir% >> %LOG_FILE% 2>&1    
  )
  if not "+%_stampfilepath%"=="+." (
    rem Then generate a stamp file if required    
    if not exist %_output% mkdir %_output%
    echo. > %_stampfilepath%
  )
  if "+%_behaviour%"=="+%PROCESSPKG_TOOLCHAIN%" (
    if exist %_binpackageoutputdir%\%_pkgname%-%_pkgvariant%-bin.7z (
      del %_binpackageoutputdir%\%_pkgname%-%_pkgvariant%-bin.7z
    )
    ren %_binpackageoutputdir%\%_pkgbasefilename%.7z %_pkgname%-%_pkgvariant%-bin.7z
  )
)
if "%_warn_if_package_not_found%"=="0" goto unpack_exit
if not exist %_input% (
  call :warn Package not found: %_pkgdisplayname% ^(%_pkgver%^)
)
:unpack_exit
endlocal
goto :EOF

:copybinary
setlocal EnableDelayedExpansion
set _result=1
set _name=%2
set _bit=%3
set _src=%4
set _target=%5
set _upx_optional_switches=%6
call :log * Building Project: %_name% ...
rem Assume that we will build an EXE file...
set _fname=%_name%.exe 
set _project=%_src%\%_name%\src\%_name%.lpi
set _binary=%_src%\%_name%\bin\%_fname%
if not exist "%_project%" (
  set _project=%_src%\src\%_name%.lpi  
  set _binary=%_src%\bin\%_fname%
)
if not exist "%_project%" (
  call :err Missing Project: "%_name%".
  set _result=0
  goto copybinaryexit
)
set LAZDEBUG=
if "+%DEBUG_MODE%"=="+1" set LAZDEBUG=--verbose
set LAZCPU=i386
set LAZOS=win32
if "%_bit%+"=="64+" (
  set LAZCPU=x86_64
  set LAZOS=win64
)
%LAZBUILD% %_project% --build-mode="Release" --cpu=%LAZCPU% --operating-system=%LAZOS% %LAZDEBUG% >> %LOG_FILE% 2>&1
if "$!errorlevel!"=="$0" goto checkbuildoutput
call :err Failing Building Project: "%_name%".
set _result=0
goto copybinaryexit
:checkbuildoutput
rem Check if a DLL file has been generated (instead of a EXE file...)
if exist %_src%\%_name%\bin\%_name%.dll (
  set _fname=%_name%.dll
  set _binary=%_src%\%_name%\bin\!_fname!
)
if exist %_src%\bin\%_name%.dll (
  set _fname=%_name%.dll
  set _binary=%_src%\bin\!_fname!
)
:copybinarybuild
copy /B %_binary% %_target% >> %LOG_FILE% 2>&1
:copybinarycheck
set _mode=
set _tmpfile="%TEMP%\%_name%.tmp"
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
%UPXPACK% -9 %_upx_optional_switches% %_target%\%_fname% >> %LOG_FILE% 2>&1
if "%SIGN_BINARIES%+"=="1+" (
    call %DUALSIGN% %_target%\%_fname% >> %LOG_FILE% 2>&1
    if "$!errorlevel!"=="$0" goto copybinaryexit
    call :err Failing Signing Project: "%_name%".
    set _result=0
    goto copybinaryexit
)
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
set _python_buffer_temp="%TEMP%\pythonver.tmp"
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

:buildpkgcache
setlocal EnableDelayedExpansion
:buildpkgcache_setvars
set _basedir=%SETUP_PACKAGES_INPUT_DIR%
set _dircachefn=directories.txt
set _pkglistfn=packages.txt
set _pkgcacheurl=%PACKAGES_CACHE_URL%
if not "%_pkgcacheurl:~-1%"=="/" set "_pkgcacheurl=%PACKAGES_CACHE_URL%/"
set _pkgdirsurl=%_pkgcacheurl%%_dircachefn%
set _pkglisturl=%_pkgcacheurl%%_pkglistfn%
set _dircachepath=%_basedir%\%_dircachefn%
set _pkglistpath=%_basedir%\%_pkglistfn%
:buildpkgcache_init
if not exist "%_basedir%" (
  mkdir "%_basedir%" 2>nul || (
    call :err * Packages Cache: Unable to create packages cache directory.
    call :log * Directory: "%_basedir%"
    goto :eof
  )
)
:buildpkgcache_download
if exist %_dircachepath% del %_dircachepath%
%WGET% -q -O "%_dircachepath%" "%_pkgdirsurl%"
if not exist "%_dircachepath%" (
  call :err * Packages Cache: Unable to download directories list.
  call :log * URL: "%_pkgdirsurl%"
  call :log * Destination: "%_dircachepath%"
  goto buildpkgcache_exit
)
:buildpkgcache_mkdirs
for /f "usebackq tokens=* delims=" %%a in ("%_dircachepath%") do (
  set "line=%%a"
  if not "!line:~0,1!"=="#" (
    set "target_dir=%_basedir%\!line!"  
    if not exist "!target_dir!" (
      mkdir "!target_dir!"
      if !errorlevel! neq 0 (
        call :err * Packages Cache: Unable to create directory.
        call :log * Directory: "!target_dir!"
        goto buildpkgcache_exit		
      )
    )
  )
)
:buildpkgcache_dlpackages
if exist %_pkglistpath% del %_pkglistpath%
%WGET% -q -O "%_pkglistpath%" "%_pkglisturl%"
if not exist "%_pkglistpath%" (
  call :err Packages Cache: Unable to download packages list.
  goto buildpkgcache_exit
)
for /f "usebackq tokens=* delims=" %%a in ("%_pkglistpath%") do (
  set "line=%%a"
  if not "!line!"=="" if not "!line:~0,1!"=="#" (   
    rem Extract relative path by removing base URL
    set "pkg_url=!line!"
    set "rel_path=!pkg_url:%PACKAGES_CACHE_URL%=!"
    if "!rel_path:~0,1!"=="/" set "rel_path=!rel_path:~1!"
    set "rel_path=!rel_path:/=\!"  
    rem Determine destination path
    set "dest_path=%_basedir%\!rel_path!"    
    rem Create parent directory if needed
    for %%i in ("!dest_path!") do set "dest_dir=%%~dpi"
    if not exist "!dest_dir!" mkdir "!dest_dir!" 2>nul   
    rem Download file only if it doesn't exist
    if not exist "!dest_path!" (
      call :log * Downloading: !pkg_url!
      %WGET% -q -O "!dest_path!" "!pkg_url!"
      if !errorlevel! neq 0 (
        call :err Packages Cache: Failed to download package.
        call :log URL: "!pkg_url!"
        call :log Destination: "!dest_path!"
      )
    )
  )
)
:buildpkgcache_exit
if exist %_pkglistpath% del %_pkglistpath%
if exist %_dircachepath% del %_dircachepath%
endlocal
goto :EOF

:processtoolchains
setlocal EnableDelayedExpansion
set "_toolchain_arch=%~1"
set "_toolchain_profiles=%~2"
set "_toolchain_profiles=!_toolchain_profiles:;= !"
set "_toolchain_processpkg_type=%PROCESSPKG_TOOLCHAIN%"
if "%_toolchain_arch%"=="64" set "_toolchain_processpkg_type=%PROCESSPKG_TOOLCHAIN64%"
rem Check if arch is unknown
if not "%_toolchain_arch%"=="32" if not "%_toolchain_arch%"=="64" (
  call :err Architecture must be "32" or "64".
  goto :eof
)
rem For all profiles, retrieve the sh-elf and arm-eabi values then call the :processpkg func
for %%v in (%_toolchain_profiles%) do (
  rem Local variables for the current iteration
  set "_current_profile=%%v"  
  set "_profile_name="
  set "_armeabi_value="
  set "_shelf_value="
  set "_profile_key=%%v"
  call :tolower _profile_key  
  rem Extract the values using the keys from the INI file
  for /f "usebackq tokens=1,2 delims==" %%A in ("%PACKAGES_CONFIG_FILE%") do (
    set "_key=%%A"
    set "_value=%%B"
    rem Trim spaces before and after the key
    set "_key=!_key: =!"    
    rem Extract the value from the key if the pattern is found
    if "!_key!"=="TOOLCHAINS!_toolchain_arch!_VERSION_PACKAGE_ARMEABI_!_current_profile!" (
      set "_armeabi_value=!_value!"
    )
    if "!_key!"=="TOOLCHAINS!_toolchain_arch!_VERSION_PACKAGE_SHELF_!_current_profile!" (
      set "_shelf_value=!_value!"
    )
    if "!_key!"=="TOOLCHAINS!_toolchain_arch!_VERSION_NAME_!_current_profile!" (
      set "_profile_name=!_value!"
    )
  )
  rem !_profile_name! should be used...
  rem Execute :processpkg if all the necessary keys were found
  if defined _armeabi_value (
    call :processpkg !_toolchain_processpkg_type! arm-eabi-toolchain !_armeabi_value! !_profile_key! toolchain arm-eabi
  ) else (
    call :err Toolchains: arm-eabi value not found for !_current_profile!
  )
  if defined _shelf_value (
    call :processpkg !_toolchain_processpkg_type! sh-elf-toolchain !_shelf_value! !_profile_key! toolchain sh-elf
  ) else (
    call :err Toolchains: sh-elf value not found for !_current_profile!
  )
)
endlocal
goto :EOF

:processgdb
setlocal EnableDelayedExpansion
set "_gdb_arch=%~1"
set "_gdb_version=%~2"
set "_toolchain_processpkg_type=%PROCESSPKG_TOOLCHAIN_OPTIONAL%"
if "%_gdb_arch%"=="64" set "_toolchain_processpkg_type=%PROCESSPKG_TOOLCHAIN_OPTIONAL64%"
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% no-python
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-2.7
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.0
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.1
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.2
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.3
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.4
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.5
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.6
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.7
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.8
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.9
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.10
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.11
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.12
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.13
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.14
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.15
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.16
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.17
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.18
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.19
call :processpkg %_toolchain_processpkg_type% sh-elf-gdb %_gdb_version% python-3.20
endlocal
goto :EOF
