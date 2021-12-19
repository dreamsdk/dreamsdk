rem @echo off
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

rem Check if DreamSDK is installed (of course, you can use a previous version!)
if "$%DREAMSDK_HOME%"=="$" goto err_dreamsdk_missing

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\offline.ini
for /F "tokens=*" %%i in (%CONFIG_FILE%) do (
	set %%i 2> nul
)

rem Sanitize configuration entries
call :trim GIT
call :trim PYTHON
call :trim SEVENZIP
call :trim SEVENZIP_COMPRESSION_LEVEL
call :trim OUTPUT_DIR
call :trim KALLISTI_URL
call :trim KALLISTI_PORTS_URL
call :trim DREAMCAST_TOOL_SERIAL_URL
call :trim DREAMCAST_TOOL_INTERNET_PROTOCOL_URL
call :trim RUBY_URL
call :trim RUBY_SAMPLE_DREAMPRESENT_URL
call :trim RUBY_SAMPLE_MRBTRIS_URL

rem Utilities
set PYREPL="%PYTHON%" "%BASE_DIR%\data\pyrepl.py"
set RUNNER="%DREAMSDK_HOME%\msys\1.0\opt\dreamsdk\dreamsdk-runner.exe"
set PATCH="%DREAMSDK_HOME%\msys\1.0\bin\patch.exe"

rem Temporary Directory
set INPUT_DIR=%BASE_DIR%\.working
if not exist "%INPUT_DIR%" mkdir %INPUT_DIR%
attrib +H "%INPUT_DIR%"

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

rem Ruby Directories
set RUBY_INPUT_DIR=%INPUT_DIR%\ruby
set RUBY_OUTPUT_DIR=%OUTPUT_DIR%\lib-embedded\ruby

set RUBY_MRUBY_INPUT_DIR=%RUBY_INPUT_DIR%\mruby
set RUBY_MRUBY_OUTPUT_DIR=%RUBY_OUTPUT_DIR%\mruby
set RUBY_SAMPLES_INPUT_DIR=%RUBY_INPUT_DIR%\samples
set RUBY_SAMPLES_OUTPUT_DIR=%RUBY_OUTPUT_DIR%\samples
set RUBY_DREAMPRESENT_INPUT_DIR=%RUBY_SAMPLES_INPUT_DIR%\dreampresent
set RUBY_DREAMPRESENT_OUTPUT_DIR=%RUBY_SAMPLES_OUTPUT_DIR%\dreampresent
set RUBY_MRBTRIS_INPUT_DIR=%RUBY_SAMPLES_INPUT_DIR%\mrbtris
set RUBY_MRBTRIS_OUTPUT_DIR=%RUBY_SAMPLES_OUTPUT_DIR%\mrbtris

rem Source Packages Directory
set SOURCE_PACKAGES_OUTPUT_DIR=%OUTPUT_DIR%\source-packages

:start
pushd .

:check_python
set PYTHON_VERSION_MAJOR=
set PYTHON_VERSION=
call :get_version_python PYTHON_VERSION_MAJOR PYTHON_VERSION
if "$%PYTHON_VERSION_MAJOR%"=="$3" goto check_git
goto err_binary_python

:check_git
set GIT_VERSION=
call :get_version_git GIT_VERSION
if "$%GIT_VERSION%"=="$" goto err_binary_git

:checkinputdirs
if not exist %INPUT_DIR% mkdir %INPUT_DIR%
if not exist %LIB_INPUT_DIR% mkdir %LIB_INPUT_DIR%
if not exist %DCLOAD_INPUT_DIR% mkdir %DCLOAD_INPUT_DIR%
if not exist %RUBY_INPUT_DIR% mkdir %RUBY_INPUT_DIR%
if not exist %RUBY_SAMPLES_INPUT_DIR% mkdir %RUBY_SAMPLES_INPUT_DIR%

rem Additional files
set KOS_ENVIRON=environ.sh

goto kos

rem KallistiOS
:kos
call :getver VERSION_KOS %KOS_INPUT_DIR%
call :log Processing: KallistiOS (%VERSION_KOS%)
call :git %LIB_INPUT_DIR% kos %KALLISTI_URL%
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

call :getver VERSION_KOS_PORTS %KOS_PORTS_INPUT_DIR%
call :log Processing: KallistiOS Ports (%VERSION_KOS_PORTS%)
call :remove_dir_tree %KOS_PORTS_OUTPUT_DIR%
call :git %LIB_INPUT_DIR% kos-ports %KALLISTI_PORTS_URL%

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
mkdir %LIB_OUTPUT_DIR%\dcload

rem Dreamcast-Tool IP
:dcload_ip
call :getver VERSION_DCLOAD_IP %DCLOAD_IP_INPUT_DIR%
call :log Processing: Dreamcast-Tool Internet Protocol (%VERSION_DCLOAD_IP%)
call :remove_dir_tree %DCLOAD_IP_OUTPUT_DIR%
call :git %DCLOAD_INPUT_DIR% dcload-ip %DREAMCAST_TOOL_INTERNET_PROTOCOL_URL%
call :copy %DCLOAD_IP_INPUT_DIR% %DCLOAD_IP_OUTPUT_DIR% %VERSION_DCLOAD_IP%
call :packsrc dcload-ip %DCLOAD_IP_OUTPUT_DIR%

rem Dreamcast-Tool Serial
:dcload_serial
call :getver VERSION_DCLOAD_SERIAL %DCLOAD_SER_INPUT_DIR%
call :log Processing: Dreamcast-Tool Serial (%VERSION_DCLOAD_SERIAL%)
call :remove_dir_tree %DCLOAD_SER_OUTPUT_DIR%
call :git %DCLOAD_INPUT_DIR% dcload-serial %DREAMCAST_TOOL_SERIAL_URL%
call :copy %DCLOAD_SER_INPUT_DIR% %DCLOAD_SER_OUTPUT_DIR% %VERSION_DCLOAD_SERIAL%
call :packsrc dcload-serial %DCLOAD_SER_OUTPUT_DIR%
goto ruby

rem Ruby: mruby
:ruby
call :getver VERSION_RUBY %RUBY_MRUBY_INPUT_DIR%
call :log Processing: Ruby (%VERSION_RUBY%)
call :remove_dir_tree %RUBY_OUTPUT_DIR%
call :git %RUBY_INPUT_DIR% mruby %RUBY_URL%
call :copy %RUBY_MRUBY_INPUT_DIR% %RUBY_MRUBY_OUTPUT_DIR% %VERSION_RUBY%
call :packsrc ruby %RUBY_OUTPUT_DIR%

rem Ruby: dreampresent
:ruby_dreampresent
call :getver VERSION_DREAMPRESENT %RUBY_DREAMPRESENT_INPUT_DIR%
call :log Processing: Ruby Sample: DreamPresent (%VERSION_DREAMPRESENT%)
call :remove_dir_tree %RUBY_DREAMPRESENT_OUTPUT_DIR%
call :git %RUBY_SAMPLES_INPUT_DIR% dreampresent %RUBY_SAMPLE_DREAMPRESENT_URL%
call :copy %RUBY_DREAMPRESENT_INPUT_DIR% %RUBY_DREAMPRESENT_OUTPUT_DIR% %VERSION_DREAMPRESENT%

rem Ruby: mrbtris
:ruby_mrbtris
call :getver VERSION_MRBTRIS %RUBY_MRBTRIS_INPUT_DIR%
call :log Processing: Ruby Sample: Mrbtris (%VERSION_MRBTRIS%)
call :remove_dir_tree %RUBY_MRBTRIS_OUTPUT_DIR%
call :git %RUBY_SAMPLES_INPUT_DIR% mrbtris %RUBY_SAMPLE_MRBTRIS_URL%
call :copy %RUBY_MRBTRIS_INPUT_DIR% %RUBY_MRBTRIS_OUTPUT_DIR% %VERSION_MRBTRIS%

:ruby_compress_samples
call :packsrc samples %RUBY_SAMPLES_OUTPUT_DIR%

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

:err_binary_python
call :log Python 3 was not found in your PATH.
goto end

:err_binary_git
call :log Git was not found in your PATH.
goto end

:err_dreamsdk_missing
call :log Please install DreamSDK to use this script.
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
echo %3 > %2\OFFLINE
goto :EOF

:getver
set tmpgetver=(UNKNOWN)
set tmpverfile=%1.tmp
%GIT% -C "%2" describe --always --tags > %tmpverfile%
if not exist %tmpverfile% goto getverend
setlocal EnableDelayedExpansion
set /p tmpgetver=<%tmpverfile%
set tmpgetver=%tmpgetver%-offline
del %tmpverfile%
:getverend
endlocal & set %1=%tmpgetver%
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
%SEVENZIP% a -t7z -mx%SEVENZIP_COMPRESSION_LEVEL% "%_target_dir%\%_package_name%" "%_source_dir%\*"
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
%PATCH% -N -d %1 -p1 -r - < %2 >> %LOG_FILE% 2>&1
goto :EOF

:patchreverse
%PATCH% --reverse -N -d %1 -p1 -r - < %2 >> %LOG_FILE% 2>&1
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
if exist %_remove_dir_tree% rmdir /S "%_remove_dir_tree%" /Q
:remove_dir_tree_exit
endlocal
goto :EOF

:get_version_git
rem Check if Git is installed
rem Usage: call :get_version_git GIT_VERSION
setlocal EnableDelayedExpansion
set _git_exec=git.exe
set _git_installed=0
set _git_version=
set _git_buffer_temp=%_git_exec%_buffer.tmp
call :check_command %_git_exec% _git_installed
if "%_git_installed%"=="0" goto get_version_git_exit
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
set _python_exec=python.exe
set _python_installed=0
set _python_version_major=
set _python_version=
set _python_buffer_temp=%_python_exec%_buffer.tmp
call :check_command %_python_exec% _python_installed
if "%_python_installed%"=="0" goto get_version_python_exit
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
endlocal & set "%~2=%_cmdfound%"
goto :EOF
