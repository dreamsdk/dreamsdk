@echo off
title DreamSDK :: Repository Initialization
echo Initializing DreamSDK repository...
git pull
git submodule update --init --recursive
pause
