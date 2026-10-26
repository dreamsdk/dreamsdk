@echo off
title DreamSDK :: Cleanup Submodules
pushd "%~dp0"
rem Revert the version bump made by the Prepare script (the "ProductVersion"
rem and "LegalCopyright" of each Lazarus project and the matching compiled
rem resources) so the submodules don't stay dirty
echo Looking for version changes made by the Prepare script...
set "_list=%TEMP%\dreamsdk-cleanup.tmp"
git submodule foreach --quiet --recursive "for f in $(git diff --name-only -- '*.lpi' '*.res'); do echo $displaypath/$f; done" > "%_list%"
for %%i in ("%_list%") do if %%~zi equ 0 (
  echo Nothing to clean up.
  del "%_list%"
  goto end
)
type "%_list%"
del "%_list%"
echo.
echo WARNING: any other change made to these files will be lost.
choice /c YN /m "Revert these files"
if errorlevel 2 goto end
git submodule foreach --quiet --recursive "git diff --name-only -z -- '*.lpi' '*.res' | xargs -0 -r git checkout --"
echo Done.
:end
popd
if not "%~1"=="/nopause" pause
