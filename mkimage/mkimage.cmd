@echo off
set APP_TITLE=DreamSDK Setup Image Builder
title %APP_TITLE%
cls

rem Initialization
set BASE_NAME=mkimage
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set LOG_FILE=%BASE_DIR%\%BASE_NAME%.log
if exist %LOG_FILE% del %LOG_FILE%

call :log %APP_TITLE%
call :log

:init
rem Some temp directory and files
set TEMP_RESULT_FILE=%TEMP%\~%BASE_NAME%.tmp

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\%BASE_NAME%.ini
if not exist "%CONFIG_FILE%" goto err_config
for /f "tokens=*" %%i in (%CONFIG_FILE%) do (
  set %%i 2> nul
  rem Sanitize configuration entry
  for /f "tokens=1 delims==" %%j in ("%%i") do (
    call :trim %%j
  )
)

rem Initilization
set SETUP_SOURCE_DIR=%SETUP_INPUT_DIR%\bin

rem Utilities
set MKISOFS="%DREAMSDK_HOME%\msys\1.0\bin\mkisofs.exe"
set CDI4DC="%DREAMSDK_HOME%\msys\1.0\bin\cdi4dc.exe"
set GETVER="%PYTHON%" "%BASE_DIR%\data\getver.py" "%SETUP_SOURCE_DIR%\setup.exe"
set UPPER="%PYTHON%" "%BASE_DIR%\data\upper.py"
set GENSORT="%PYTHON%" "%BASE_DIR%\data\gensort.py"
set RUNNER="%DREAMSDK_HOME%\msys\1.0\opt\dreamsdk\dreamsdk-runner.exe"

:check_input
rem Input Directory
if not exist %SETUP_INPUT_DIR% goto err_input_dir

rem Generated DreamSDK Setup
if not exist %SETUP_SOURCE_DIR% goto err_setup_not_generated

:check_output
rem Output Directory
set SETUP_OUTPUT_DIR=%BASE_DIR%\dist
if not exist %SETUP_OUTPUT_DIR% mkdir %SETUP_OUTPUT_DIR%

set DCLOAD_INPUT_DIR=%BASE_DIR%\.dcload
if not exist %DCLOAD_INPUT_DIR% mkdir %DCLOAD_INPUT_DIR%

set IMAGE_OUTPUT_DIR=%BASE_DIR%\.cd_root
if not exist %IMAGE_OUTPUT_DIR% mkdir %IMAGE_OUTPUT_DIR%

:check_tools
if not exist %MKISOFS% goto err_binary_mkisofs
if not exist %CDI4DC% goto err_binary_cdi4dc

:check_python
set PYTHON_VERSION_MAJOR=
set PYTHON_VERSION=
call :get_version_python PYTHON_VERSION_MAJOR PYTHON_VERSION
if "$%PYTHON_VERSION_MAJOR%"=="$3" goto start
goto err_binary_python

rem Do the magic!

:start
pushd .

:prepare_disc_root
call :copy %SETUP_SOURCE_DIR% %IMAGE_OUTPUT_DIR%

:extract_versioninfo
rem Extract product information from the Setup file...
%GETVER% ProductVersion > %TEMP_RESULT_FILE%
set /p PACKAGE_RELEASE_VERSION=< %TEMP_RESULT_FILE%
 
rem Extract product name from the Setup file...
%GETVER% ProductName > %TEMP_RESULT_FILE%
set /p PACKAGE_NAME=< %TEMP_RESULT_FILE%

rem Extract company from the Setup file...
%GETVER% CompanyName > %TEMP_RESULT_FILE%
set /p PUBLISHER=< %TEMP_RESULT_FILE%

rem Extract file version from the Setup file...
%GETVER% FileVersion > %TEMP_RESULT_FILE%
set /p PACKAGE_BUILD_VERSION=< %TEMP_RESULT_FILE%

rem Getting the current timestamp
call :get_timestamp TIMESTAMP

:setting_up_parameters
rem Settings for final Setup image...
set SYSID=Win32
set PREPARER=%PACKAGE_NAME% Setup Generator
set APPID=%PACKAGE_NAME% %PACKAGE_RELEASE_VERSION%
call :log Building: %APPID% (%PACKAGE_BUILD_VERSION%)

rem Generate valid Volume ID
set VOLUMEID=%PACKAGE_NAME%_%PACKAGE_RELEASE_VERSION%
%RUNNER% "./data/genvolid.sh %VOLUMEID%" > %TEMP_RESULT_FILE%
set /p VOLUMEID=< %TEMP_RESULT_FILE%

rem Output files
set SETUP_OUTPUT_BASE_FILE=%PACKAGE_NAME%-%PACKAGE_RELEASE_VERSION%-Setup
set SETUP_OUTPUT_ISO_FILE=%SETUP_OUTPUT_BASE_FILE%.iso
set SETUP_OUTPUT_ISO_PATH=%SETUP_OUTPUT_DIR%\%SETUP_OUTPUT_ISO_FILE%
set DISC_ID_DIZ=%IMAGE_OUTPUT_DIR%\disc_id.diz

:check_iso
if exist %SETUP_OUTPUT_ISO_PATH% goto err_generated_iso

:make_autorun_inf
call :generate_autorun_inf "%APPID%"

:adding_extra_files
set DREAMSDK_INPUT_DIR=%SYSTEM_OBJECTS_INPUT_DIR%\mingw\msys\1.0\opt\dreamsdk
copy /B %DREAMSDK_INPUT_DIR%\getstart.rtf %IMAGE_OUTPUT_DIR%\readme.rtf >> %LOG_FILE% 2>&1
copy /B %DREAMSDK_INPUT_DIR%\LICENSE %IMAGE_OUTPUT_DIR%\license.txt >> %LOG_FILE% 2>&1
copy /B %DOCUMENTATION_INPUT_DIR%\bin\dreamsdk.chm %IMAGE_OUTPUT_DIR%\dreamsdk.chm >> %LOG_FILE% 2>&1
echo %SETUP_OUTPUT_BASE_FILE% > %DISC_ID_DIZ%
echo Build %PACKAGE_BUILD_VERSION% (%TIMESTAMP%) >> %DISC_ID_DIZ%

:generate_iso
rem Generate Setup program
call :log Generating: ISO Image
%MKISOFS% -V "%VOLUMEID%" -sysid "%SYSID%" -publisher "%PUBLISHER%" -preparer "%PREPARER%" -appid "%APPID%" -duplicates-once -joliet -rational-rock -full-iso9660-filenames -o "%SETUP_OUTPUT_ISO_PATH%" "%IMAGE_OUTPUT_DIR%" >> %LOG_FILE% 2>&1
if "%errorlevel%+"=="0+" goto generate_cdi_dcload_ip
goto err_generation

:generate_cdi_dcload_ip
if "%GENERATE_DREAMCAST_TOOL_SERIAL_IMAGE%+"=="1+" (
  call :generate_cdi "Internet Protocol" dcload-ip %DREAMCAST_TOOL_INTERNET_PROTOCOL_URL%
)

:generate_cdi_dcload_serial
if "%GENERATE_DREAMCAST_TOOL_INTERNET_PROTOCOL_IMAGE%+"=="1+" (
  call :generate_cdi "Serial" dcload-serial %DREAMCAST_TOOL_SERIAL_URL%
)

:finish
call :log
call :log Done!
call :log
goto end

:end
popd
if exist "%TEMP_RESULT_FILE%" del "%TEMP_RESULT_FILE%"
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

:err_setup_not_generated
call :err The DreamSDK Setup has not been generated. You need to generate it.
call :log Directory: "%SETUP_SOURCE_DIR%"
goto end

:err_binary_mkisofs
call :err Make ISO Image File System (mkisofs) was not found.
call :log Do you have DreamSDK installed?
call :log File: "%MKISOFS%"
goto end

:err_binary_cdi4dc
call :err CDI4DC was not found.
call :log Do you have DreamSDK installed?
call :log File: "%CDI4DC%"
goto end

:err_binary_python
call :err Python 3 was not found.
call :log File: "%PYTHON%"
goto end

:err_generation
call :err Unable to generate the ISO file.
call :log File: "%SETUP_OUTPUT_ISO_FILE%"
goto end

:err_generated_iso
call :err ISO file was already generated.
call :log File: "%SETUP_OUTPUT_ISO_FILE%"
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

:getver
set tmpgetver=(UNKNOWN)
set tmpverfile=%1.tmp
%GIT% -C "%2" describe --always > %tmpverfile%
if not exist %tmpverfile% goto getverend
setlocal EnableDelayedExpansion
set /p tmpgetver=<%tmpverfile%
set tmpgetver=%tmpgetver%
del %tmpverfile%
:getverend
endlocal & set %1=%tmpgetver%
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

:generate_cdi
setlocal EnableDelayedExpansion
set _name=%~1
set _codename=%2
set _giturl=%3
set _outdir=%DCLOAD_INPUT_DIR%\%_codename%
set _makefile_conf=Makefile.cfg

call :log Generating: CDI Image: Dreamcast-Tool %_name%

:generate_cdi_dcload_get_source
call :git %DCLOAD_INPUT_DIR% %_codename% %_giturl%
call :get_makefile_version _dcload_version %_outdir%\%_makefile_conf%
call :getver _version %_outdir%
set _version=%_dcload_version%-%_version%
set _radicalfn=%SETUP_OUTPUT_BASE_FILE%-%_codename%-%_version%
set _friendly_volume_label=%APPID% (%_codename%)
set _volume_label=%VOLUMEID%

:generate_cdi_check_output
set _target_file=%_radicalfn%.cdi
set _target_file_path=%SETUP_OUTPUT_DIR%\%_target_file%
if exist %_target_file_path% (
  call :err CDI file was already generated.
  call :log File: "%_target_file%"
  goto end
)

:generate_cdi_make_package
%SEVENZIP% a -xr^^!.git\ -xr^^!*~ -mx9 "%IMAGE_OUTPUT_DIR%\dcload.zip" %_outdir%\* >> %LOG_FILE% 2>&1

:generate_cdi_dcload_make
set _makefile_backup=Makefile.bak
set _makefile_temp=Makefile.tmp
cd %_outdir%
if not exist %_makefile_backup% (
  %RUNNER% "sed -e ""s/#STANDALONE_BINARY/STANDALONE_BINARY/g"" %_makefile_conf%" > %_makefile_temp%
  ren %_makefile_conf% %_makefile_backup%
  move %_makefile_temp% %_makefile_conf% >> %LOG_FILE% 2>&1
)
%RUNNER% "make" >> %LOG_FILE% 2>&1

:generate_cdi_prepare_disc
call :generate_autorun_inf "%_friendly_volume_label%"
set _dctool_binary_client_file=%IMAGE_OUTPUT_DIR%\dc-tool.exe
set _dctool_binary_client_unix_file=%_dctool_binary_client_file%
copy /B %_outdir%\target-src\1st_read\1st_read.bin %IMAGE_OUTPUT_DIR% >> %LOG_FILE% 2>&1
copy /B %_outdir%\make-cd\IP.BIN %IMAGE_OUTPUT_DIR% >> %LOG_FILE% 2>&1
copy /B %_outdir%\host-src\tool\dc-tool*.exe %_dctool_binary_client_file% >> %LOG_FILE% 2>&1
call :win2unix _dctool_binary_client_unix_file
%RUNNER% "strip ""%_dctool_binary_client_unix_file%"""
%UPX32% -9 "%_dctool_binary_client_file%" >> %LOG_FILE% 2>&1
echo %_radicalfn% > %DISC_ID_DIZ%
echo Build %PACKAGE_BUILD_VERSION% >> %DISC_ID_DIZ%
%UPPER% %IMAGE_OUTPUT_DIR%

:generate_cdi_make_disc
set SORT_FILE=%BASE_DIR%\sortfile.str
%GENSORT% %IMAGE_OUTPUT_DIR% > %SORT_FILE%
call :win2unix _target_file_path
set SOURCE_DIR=%IMAGE_OUTPUT_DIR%
call :win2unix SOURCE_DIR
set BOOTSTRAP_FILE=%IMAGE_OUTPUT_DIR%\IP.BIN
call :win2unix BOOTSTRAP_FILE
call :win2unix SORT_FILE
%RUNNER% "makedisc %_target_file_path% %SOURCE_DIR% %BOOTSTRAP_FILE% %_volume_label% --data --joliet-rock %SORT_FILE%" >> %LOG_FILE% 2>&1
endlocal
goto :EOF

:copy
set EXCLUDE_FILE=%BASE_DIR%\exclude.txt
echo .git\ > %EXCLUDE_FILE%
echo .svn\ >> %EXCLUDE_FILE%
xcopy %1\* %2 /exclude:%EXCLUDE_FILE% /s /i /y >> %LOG_FILE% 2>&1
if exist %EXCLUDE_FILE% del %EXCLUDE_FILE%
goto :EOF

:get_makefile_version
setlocal EnableDelayedExpansion
set _makefile=%2
set _version=
call :win2unix _makefile
%RUNNER% "./data/getmkvar.sh %_makefile% VERSION" > %TEMP_RESULT_FILE%
set /p _version=< %TEMP_RESULT_FILE%
endlocal & (
	set "%~1=%_version%"
)
goto :EOF

:generate_autorun_inf
setlocal EnableDelayedExpansion
set _label=%~1
set _autorun_inf=%IMAGE_OUTPUT_DIR%\autorun.inf
echo [autorun] > %_autorun_inf%
echo icon=setup.exe >> %_autorun_inf%
echo open=setup.exe >> %_autorun_inf%
echo label=%_label% >> %_autorun_inf%
endlocal
goto :EOF

rem Thanks to: https://stackoverflow.com/a/36410527/3726096
:get_timestamp
setlocal EnableDelayedExpansion
:: put your desired field _delimiter here.
:: for example, setting _delimiter to a hyphen will separate fields like so:
:: yyyy-MM-dd_hh-mm-ss
::
:: setting _delimiter to nothing will output like so:
:: yyyyMMdd_hhmmss
::
set _part_delimiter=%2
set _delimiter=%3

set _datestring=%date:~-4,4%%_delimiter%%date:~-7,2%%_delimiter%%date:~-10,2%
set _timestring=%TIME%
::TRIM OFF the LAST 3 characters of _timestring, which is the decimal point and hundredths of a second
set _timestring=%_timestring:~0,-3%

:: Replace colons from _timestring with _delimiter
set _timestring=%_timestring::=!_delimiter!%

:: if there is a preceeding space substitute with a zero
set _result=%_datestring%%_part_delimiter%%_timestring: =0%
endlocal & set %1=%_result%
goto :EOF
