@echo off
title DreamSDK :: Update Submodules
call "%~dp0cleanup.cmd" /nopause
title DreamSDK :: Update Submodules
echo.
echo Updating DreamSDK submodules...
git submodule update --remote --merge
pause
