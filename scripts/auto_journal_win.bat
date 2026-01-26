@echo off
REM Auto-Journal Windows Launcher
REM Run from Windows side to capture Windows screenshots

SET SCRIPT_DIR=%~dp0
for /f %%i in ('wsl -d Ubuntu -e bash -lc "echo $HOME"') do set WSL_HOME=%%i
if "%SWIMMY_HOME%"=="" set SWIMMY_HOME=%WSL_HOME%/swimmy
SET WSL_PATH=%SWIMMY_HOME%

REM Run via WSL with DISPLAY set for Windows
wsl -d Ubuntu -e bash -lc "cd %WSL_PATH% && source .venv/bin/activate && DISPLAY=:0 python -m auto_journal.auto_journal %*"
