@echo off
set APP_TITLE=KallistiOS Offline Packager for DreamSDK Setup
title %APP_TITLE%
cls

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set LOG_FILE=%BASE_DIR%\offline.log
if exist %LOG_FILE% del %LOG_FILE%

call :log %APP_TITLE%
call :log

:init
rem Check if DreamSDK is installed (of course, you can use a previous version!)
if "$%DREAMSDK_HOME%"=="$" goto err_dreamsdk_missing

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\offline.ini
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
if not exist %PATCH% set PATCH="%DREAMSDK_HOME%\usr\bin\patch.exe"
set RUNNER="%DREAMSDK_HOME%\msys\1.0\opt\dreamsdk\dreamsdk-runner.exe"
if not exist %RUNNER% set RUNNER="%DREAMSDK_HOME%\opt\dreamsdk\dreamsdk-runner.exe"
set PYREPL="%PYTHON%" "%BASE_DIR%\data\pyrepl.py"
set GETDATE="%PYTHON%" "%BASE_DIR%\data\getdate.py"

rem Input Directory
call :get_temp_working_dir DreamSDK-Offline-Working INPUT_DIR

rem Output Directory
call :normalizepath SETUP_OUTPUT_DIR
if not exist "%SETUP_OUTPUT_DIR%" goto err_output_dir
set OUTPUT_DIR=%SETUP_OUTPUT_DIR%\.sources
if not exist "%OUTPUT_DIR%" mkdir %OUTPUT_DIR%

rem KallistiOS Directories
set LIB_INPUT_DIR=%INPUT_DIR%\lib
set LIB_OUTPUT_DIR=%OUTPUT_DIR%\lib-embedded\lib

set KOS_INPUT_DIR=%LIB_INPUT_DIR%\kos
set KOS_OUTPUT_DIR=%LIB_OUTPUT_DIR%\kos
set KOS_PORTS_INPUT_DIR=%LIB_INPUT_DIR%\kos-ports
set KOS_PORTS_OUTPUT_DIR=%LIB_OUTPUT_DIR%\kos-ports
set DCLOAD_INPUT_DIR=%LIB_INPUT_DIR%\dcload
set DCLOAD_OUTPUT_DIR=%LIB_OUTPUT_DIR%\dcload
set DCLOAD_IP_INPUT_DIR=%DCLOAD_INPUT_DIR%\dcload-ip
set DCLOAD_IP_OUTPUT_DIR=%DCLOAD_OUTPUT_DIR%\dcload-ip
set DCLOAD_SER_INPUT_DIR=%DCLOAD_INPUT_DIR%\dcload-serial
set DCLOAD_SER_OUTPUT_DIR=%DCLOAD_OUTPUT_DIR%\dcload-serial

rem Source Packages Directory
set SOURCE_PACKAGES_OUTPUT_DIR=%OUTPUT_DIR%\source-packages

rem Create missing Input directories
if not exist %INPUT_DIR% mkdir %INPUT_DIR%
if not exist %LIB_INPUT_DIR% mkdir %LIB_INPUT_DIR%
if not exist %DCLOAD_INPUT_DIR% mkdir %DCLOAD_INPUT_DIR%

:check_git
set GIT_VERSION=
call :get_version_git GIT_VERSION
if "$%GIT_VERSION%"=="$" goto err_binary_git

:check_sevenzip
if not exist %SEVENZIP% goto err_binary_sevenzip

:check_python
set PYTHON_VERSION_MAJOR=
set PYTHON_VERSION=
call :get_version_python PYTHON_VERSION_MAJOR PYTHON_VERSION
if "$%PYTHON_VERSION_MAJOR%"=="$3" goto start
goto err_binary_python

:start
pushd .

rem KallistiOS
:kos
set KOS_ENVIRON=environ.sh
call :log Processing: KallistiOS
call :git %LIB_INPUT_DIR% kos %KALLISTI_URL%
call :getver VERSION_KOS %KOS_INPUT_DIR%
call :log * Version: %VERSION_KOS%
call :remove_dir_tree %KOS_OUTPUT_DIR%
call :copy %KOS_INPUT_DIR% %KOS_OUTPUT_DIR% %VERSION_KOS%
if exist %KOS_OUTPUT_DIR%\%KOS_ENVIRON% del %KOS_OUTPUT_DIR%\%KOS_ENVIRON%
call :setver "KallistiOS ##version##" "KallistiOS %VERSION_KOS%" "%KOS_OUTPUT_DIR%"
call :setver "relver='##version##'" "relver='%VERSION_KOS%'" "%KOS_OUTPUT_DIR%\kernel\arch\dreamcast\kernel"
call :setver "##version##" "%VERSION_KOS%" "%KOS_OUTPUT_DIR%\doc"
call :setver "##version##" "%VERSION_KOS%" "%KOS_OUTPUT_DIR%\kernel\arch\dreamcast\hardware\pvr"
call :packsrc kallisti %KOS_OUTPUT_DIR%
goto kosports

rem KallistiOS Ports
:kosports
set KOS_PORTS_PATCH_DIR=%BASE_DIR%\data\kos-ports
set KOS_PORTS_UTILS_DIR=%KOS_PORTS_INPUT_DIR%\utils
call :log Processing: KallistiOS Ports

call :git %LIB_INPUT_DIR% kos-ports %KALLISTI_PORTS_URL%
call :getver VERSION_KOS_PORTS %KOS_PORTS_INPUT_DIR%
call :log * Version: %VERSION_KOS_PORTS%
call :remove_dir_tree %KOS_PORTS_OUTPUT_DIR%

rem Download all KallistiOS Ports at once
call :patch %KOS_PORTS_INPUT_DIR% %KOS_PORTS_PATCH_DIR%\fetch.diff

rem Downloading all
set FETCH_ALL=%KOS_PORTS_UTILS_DIR%\fetch-all.sh
call :win2unix FETCH_ALL
cd /D %KOS_PORTS_UTILS_DIR%
%RUNNER% %FETCH_ALL% >> %LOG_FILE% 2>&1
call :wait

rem Copy everything
call :copy %KOS_PORTS_INPUT_DIR% %KOS_PORTS_OUTPUT_DIR% %VERSION_KOS_PORTS%
call :patch %KOS_PORTS_OUTPUT_DIR% %KOS_PORTS_PATCH_DIR%\offline.diff
call :patchreverse %KOS_PORTS_OUTPUT_DIR% %KOS_PORTS_PATCH_DIR%\fetch.diff
call :setver "kos-ports ##version##" "kos-ports %VERSION_KOS_PORTS%" "%KOS_PORTS_OUTPUT_DIR%"
call :setver "KallistiOS ##version##" "KallistiOS %VERSION_KOS_PORTS%" "%KOS_PORTS_OUTPUT_DIR%"
call :setver "KOS ##version##" "KOS %VERSION_KOS_PORTS%" "%KOS_PORTS_OUTPUT_DIR%"
call :packsrc kallisti-ports %KOS_PORTS_OUTPUT_DIR%
goto dcload

rem Dreamcast-Tool
:dcload
title %APP_TITLE%
if not exist "%LIB_OUTPUT_DIR%\dcload" mkdir %LIB_OUTPUT_DIR%\dcload

rem Dreamcast-Tool IP
:dcload_ip
call :log Processing: Dreamcast-Tool Internet Protocol
call :git %DCLOAD_INPUT_DIR% dcload-ip %DREAMCAST_TOOL_INTERNET_PROTOCOL_URL%
call :getver VERSION_DCLOAD_IP %DCLOAD_IP_INPUT_DIR%
call :log  Version: %VERSION_DCLOAD_IP%
call :remove_dir_tree %DCLOAD_IP_OUTPUT_DIR%
call :copy %DCLOAD_IP_INPUT_DIR% %DCLOAD_IP_OUTPUT_DIR% %VERSION_DCLOAD_IP%
call :packsrc dcload-ip %DCLOAD_IP_OUTPUT_DIR%

rem Dreamcast-Tool Serial
:dcload_serial
call :log Processing: Dreamcast-Tool Serial
call :git %DCLOAD_INPUT_DIR% dcload-serial %DREAMCAST_TOOL_SERIAL_URL%
call :getver VERSION_DCLOAD_SERIAL %DCLOAD_SER_INPUT_DIR%
call :log * Version: %VERSION_DCLOAD_SERIAL%
call :remove_dir_tree %DCLOAD_SER_OUTPUT_DIR%
call :copy %DCLOAD_SER_INPUT_DIR% %DCLOAD_SER_OUTPUT_DIR% %VERSION_DCLOAD_SERIAL%
call :packsrc dcload-serial %DCLOAD_SER_OUTPUT_DIR%

:finish
call :log
call :log Done!
call :log
goto end

:end
popd
call :remove_dir_tree %INPUT_DIR%
pause
goto :EOF

rem ## Errors ##################################################################

:err_config
call :err The configuration file was not found.
call :log File: "%CONFIG_FILE%"
goto end

:err_output_dir
call :err The specified output directory (SETUP_OUTPUT_DIR) was not found.
call :log Directory: "%SETUP_OUTPUT_DIR%"
goto end

:err_binary_python
call :err Python 3 was not found.
call :log File: "%PYTHON%"
goto end

:err_binary_git
call :err Git was not found.
call :log File: "%GIT%"
goto end

:err_binary_sevenzip
call :err 7-Zip was not found.
call :log File: "%SEVENZIP%"
goto end

:err_dreamsdk_missing
call :err Please install DreamSDK before using this script.
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

:copy
set EXCLUDE_FILE=%BASE_DIR%\exclude.txt
echo .git\ > %EXCLUDE_FILE%
echo .svn\ >> %EXCLUDE_FILE%
rem if exist "%EXCLUDE_FILE%" attrib +h %EXCLUDE_FILE%
xcopy %1\* %2 /exclude:%EXCLUDE_FILE% /s /i /y >> %LOG_FILE% 2>&1
if exist %EXCLUDE_FILE% del %EXCLUDE_FILE%
echo %3 > %2\OFFLINE
goto :EOF

:getver
setlocal EnableDelayedExpansion
set _type=%1
set _dir=%2
set _getver=(UNKNOWN)

set _verfile="%TEMP%\%_type%.tmp"
%GIT% -C "%_dir%" describe --always > %_verfile%
if not exist %_verfile% goto getverend
set /p _getver=<%_verfile%
if exist %_verfile% del %_verfile%

set _verfiledomain="%TEMP%\%_type%-domain.tmp"
%GIT% -C "%_dir%" config --get remote.origin.url > %_verfiledomain%
if not exist %_verfiledomain% goto getverend
set /p _getdomain=<%_verfiledomain%
call :extract_domain_url %_getdomain% _repository_domain
if exist %_verfiledomain% del %_verfiledomain%
if not "$%_repository_domain%"=="$" set _repository_domain=%_repository_domain%-

call :today _today
set _getver=%_repository_domain%%_getver%-%_today%-offline

:getverend
endlocal & set "%1=%_getver%"
goto :EOF

:setver
%PYREPL% %1 %2 %3%
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

:packsrc
if not exist "%SOURCE_PACKAGES_OUTPUT_DIR%" mkdir %SOURCE_PACKAGES_OUTPUT_DIR%
call :compress %1-offline-src.7z %2 %SOURCE_PACKAGES_OUTPUT_DIR%
goto :EOF

:compress
setlocal EnableDelayedExpansion
set _package_name=%1
set _source_dir=%2
set _target_dir=%3
%SEVENZIP% a -t7z -mx%SEVENZIP_COMPRESSION_LEVEL% "%_target_dir%\%_package_name%" "%_source_dir%\*" >> %LOG_FILE% 2>&1
endlocal
goto :EOF

:git
rem Usage: call :git <target_path> <repo_dir> <repo_url>
setlocal EnableDelayedExpansion
set _git_target_path=%1
set _git_repo_dir=%2
set _git_repo_url=%3
if not exist "%_git_target_path%\%_git_repo_dir%" goto git_clone
goto git_pull
:git_clone
%GIT% -C "%_git_target_path%" clone %_git_repo_url% %_git_repo_dir% --verbose >> %LOG_FILE% 2>&1
goto git_exit
:git_pull
%GIT% -C "%_git_target_path%\%_git_repo_dir%" pull --verbose >> %LOG_FILE% 2>&1
:git_exit
endlocal
goto :EOF

:patch
%PATCH% --no-backup-if-mismatch --ignore-whitespace --fuzz 3 -N -d %1 -p1 -r - < %2 >> %LOG_FILE% 2>&1
goto :EOF

:patchreverse
%PATCH% --no-backup-if-mismatch --ignore-whitespace --fuzz 3 --reverse -N -d %1 -p1 -r - < %2 >> %LOG_FILE% 2>&1
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

:wait
%RUNNER% "sleep 3"
goto :EOF

:remove_dir_tree
setlocal EnableDelayedExpansion
set _remove_dir_tree=%1
if "$%_remove_dir_tree%"=="$" goto remove_dir_tree_exit
if exist %_remove_dir_tree% rmdir /S "%_remove_dir_tree%" /Q >> %LOG_FILE% 2>&1
:remove_dir_tree_exit
endlocal
goto :EOF

:get_version_git
rem Check if Git is installed
rem Usage: call :get_version_git GIT_VERSION
setlocal EnableDelayedExpansion
set _git_exec=%GIT%
set _git_installed=0
set _git_version=
set _git_buffer_temp=gitver.tmp
if exist %_git_exec% goto get_version_git_check
call :check_command %_git_exec% _git_installed
if "%_git_installed%"=="0" goto get_version_git_exit
:get_version_git_check
%_git_exec% --version > %_git_buffer_temp% 2>&1
set /p _cmd_raw_output=<%_git_buffer_temp%
if "%_cmd_raw_output:~0,11%"=="git version" goto get_version_git_install
:get_version_git_install
for /f "tokens=3 delims= " %%V in ('type %_git_buffer_temp%') do (
  set _git_version=%%V
)
:get_version_git_exit
if exist %_git_buffer_temp% del %_git_buffer_temp%
endlocal & (
	set "%~1=%_git_version%"
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

:get_temp_working_dir
rem Thanks dbenham: https://superuser.com/a/776801
setlocal EnableDelayedExpansion
set _chars=ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
:get_temp_working_dir_retry
set "_name="
for /l %%N in (1 1 8) do (
  set /a I=!random!%%36
  for %%I in (!I!) do set "_name=!_name!!_chars:~%%I,1!"
)
set "_name=%TEMP%\%1-%_name%"
if exist %_name% goto get_temp_working_dir_retry
endlocal & set "%~2=%_name%"
goto :EOF

:today
rem Thanks Joey: http://stackoverflow.com/a/10945887/1810071
setlocal EnableDelayedExpansion
set _getdatefile="%TEMP%\~getdate.tmp"
%GETDATE% > %_getdatefile%
set /p _today=<%_getdatefile%
if exist %_getdatefile% del %_getdatefile%
endlocal & set "%~1=%_today%"
goto :EOF

:extract_domain_url
setlocal EnableDelayedExpansion
set _url=%1
if "$%2"=="$" goto :EOF
for /f "tokens=2 delims=/" %%a in ("%_url%") do (
	set _subdomain_with_tld=%%a
)
for /f "tokens=1 delims=." %%b in ("%_subdomain_with_tld%") do (
	set _subdomain=%%b
)
endlocal & set "%~2=%_subdomain%"
goto :EOF
