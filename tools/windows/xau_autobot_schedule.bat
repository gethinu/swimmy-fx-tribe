@echo off
setlocal
set SCRIPT_DIR=%~dp0
powershell -NoProfile -ExecutionPolicy Bypass -File "%SCRIPT_DIR%xau_autobot_schedule.ps1" %*
exit /b %ERRORLEVEL%
