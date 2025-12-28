# Auto-Journal - Create Startup Shortcut
# Run this once to add Auto-Journal to Windows Startup folder

$StartupFolder = [Environment]::GetFolderPath('Startup')
$ShortcutPath = Join-Path $StartupFolder "SwimmyAutoJournal.lnk"
$TargetPath = "wscript.exe"
$Arguments = """\\wsl$\Ubuntu\home\swimmy\swimmy\start_journal_hidden.vbs"""

# Create shortcut
$WshShell = New-Object -ComObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut($ShortcutPath)
$Shortcut.TargetPath = $TargetPath
$Shortcut.Arguments = $Arguments
$Shortcut.Description = "Swimmy Auto-Journal"
$Shortcut.Save()

Write-Host ""
Write-Host "========================================" -ForegroundColor Green
Write-Host " Startup shortcut created!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Green
Write-Host ""
Write-Host "Location: $ShortcutPath" -ForegroundColor Cyan
Write-Host ""
Write-Host "Auto-Journal will start automatically at login." -ForegroundColor Yellow
Write-Host ""
Write-Host "To start NOW, double-click:" -ForegroundColor Cyan
Write-Host "  \\wsl`$\Ubuntu\home\swimmy\swimmy\start_journal_hidden.vbs" -ForegroundColor White
Write-Host ""
