@echo off
title DreamSDK :: Update Submodules
echo Updating DreamSDK submodules...
git pull
git submodule update --recursive
pause
