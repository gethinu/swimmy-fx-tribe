# Auto-Journal Windows Daemon Launcher
# This script runs the auto-journal daemon and restarts on failure

$ErrorActionPreference = "Continue"
$WslHome = (wsl -d Ubuntu -e bash -lc "printf %s \"$HOME\"") -replace "`r",""
$SwimmyHome = if ($env:SWIMMY_HOME) { $env:SWIMMY_HOME } else { "$WslHome/swimmy" }

Write-Host "Starting Auto-Journal Daemon..." -ForegroundColor Green

while ($true) {
    try {
        # Run auto-journal daemon via WSL
        wsl -d Ubuntu -e bash -lc "cd \"$SwimmyHome\" && source .venv/bin/activate && python -m auto_journal.auto_journal daemon"
        
        Write-Host "Daemon stopped. Restarting in 10 seconds..." -ForegroundColor Yellow
        Start-Sleep -Seconds 10
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
        Start-Sleep -Seconds 30
    }
}
