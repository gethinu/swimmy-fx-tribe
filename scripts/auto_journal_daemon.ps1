# Auto-Journal Windows Daemon Launcher
# This script runs the auto-journal daemon and restarts on failure

$ErrorActionPreference = "Continue"
$ScriptPath = "\\wsl$\Ubuntu\home\swimmy\swimmy"

Write-Host "Starting Auto-Journal Daemon..." -ForegroundColor Green

while ($true) {
    try {
        # Run auto-journal daemon via WSL
        wsl -d Ubuntu -e bash -c "cd /home/swimmy/swimmy && source .venv/bin/activate && python -m auto_journal.auto_journal daemon"
        
        Write-Host "Daemon stopped. Restarting in 10 seconds..." -ForegroundColor Yellow
        Start-Sleep -Seconds 10
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
        Start-Sleep -Seconds 30
    }
}
