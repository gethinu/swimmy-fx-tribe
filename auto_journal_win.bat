@echo off
REM Auto-Journal Windows Launcher
REM Run from Windows side to capture Windows screenshots

SET SCRIPT_DIR=%~dp0
SET WSL_PATH=/home/swimmy/swimmy

REM Run via WSL with DISPLAY set for Windows
wsl -d Ubuntu -e bash -c "cd %WSL_PATH% && source .venv/bin/activate && DISPLAY=:0 python -m auto_journal.auto_journal %*"
