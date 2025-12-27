# Auto-Journal Windows Task Scheduler Setup
# Run this script as Administrator to register auto-start

$TaskName = "SwimmyAutoJournal"
$Description = "Swimmy Auto-Journal: Screenshot capture and daily report generation"
$ScriptPath = "\\wsl$\Ubuntu\home\swimmy\swimmy\auto_journal_daemon.ps1"

# Check if running as admin
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "ERROR: Please run this script as Administrator" -ForegroundColor Red
    Write-Host "Right-click PowerShell and select 'Run as Administrator'" -ForegroundColor Yellow
    exit 1
}

# Remove existing task if exists
$existingTask = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
if ($existingTask) {
    Write-Host "Removing existing task..." -ForegroundColor Yellow
    Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
}

# Create action
$Action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-ExecutionPolicy Bypass -WindowStyle Hidden -File `"$ScriptPath`""

# Create trigger (at logon)
$Trigger = New-ScheduledTaskTrigger -AtLogon

# Create settings
$Settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable -RestartCount 3 -RestartInterval (New-TimeSpan -Minutes 1)

# Create principal (run as current user)
$Principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited

# Register the task
try {
    Register-ScheduledTask -TaskName $TaskName -Action $Action -Trigger $Trigger -Settings $Settings -Principal $Principal -Description $Description

    Write-Host ""
    Write-Host "========================================" -ForegroundColor Green
    Write-Host " Auto-Journal registered successfully!" -ForegroundColor Green
    Write-Host "========================================" -ForegroundColor Green
    Write-Host ""
    Write-Host "Task Name: $TaskName" -ForegroundColor Cyan
    Write-Host "Trigger: At user logon" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "To start now: " -NoNewline
    Write-Host "Start-ScheduledTask -TaskName '$TaskName'" -ForegroundColor Yellow
    Write-Host "To stop: " -NoNewline  
    Write-Host "Stop-ScheduledTask -TaskName '$TaskName'" -ForegroundColor Yellow
    Write-Host "To remove: " -NoNewline
    Write-Host "Unregister-ScheduledTask -TaskName '$TaskName'" -ForegroundColor Yellow
    Write-Host ""
}
catch {
    Write-Host "ERROR: Failed to register task: $_" -ForegroundColor Red
    exit 1
}
