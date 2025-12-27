@echo off
REM Auto-Journal Windows Native Daemon
REM Double-click to start capturing screenshots on Windows

REM Use pushd to handle UNC paths (maps a drive letter temporarily)
pushd "\\wsl$\Ubuntu\home\swimmy\swimmy"
if errorlevel 1 (
    echo ERROR: Cannot access WSL path
    echo Make sure WSL is running
    pause
    exit /b 1
)

echo ========================================
echo   Swimmy Auto-Journal - Windows Native
echo ========================================
echo.
echo Current directory: %CD%
echo.

REM Check Python
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python not found. Please install Python 3.
    popd
    pause
    exit /b 1
)

REM Install dependencies if needed
echo Checking dependencies...
pip show Pillow >nul 2>&1 || pip install Pillow
pip show pytesseract >nul 2>&1 || pip install pytesseract  
pip show requests >nul 2>&1 || pip install requests
pip show python-dotenv >nul 2>&1 || pip install python-dotenv
pip show pywin32 >nul 2>&1 || pip install pywin32

echo.
echo Starting Auto-Journal daemon...
echo Press Ctrl+C to stop.
echo.

:loop
python "%CD%\auto_journal_windows.py" daemon
echo.
echo Restarting in 10 seconds...
timeout /t 10 /nobreak
goto loop

REM This line won't be reached but good practice
popd
