[CmdletBinding()]
param(
    [string]$TaskName = "Start MT5",
    [string]$TerminalPath = "",
    [string]$TerminalArguments = "",
    [string]$RunAsUser = "",
    [switch]$IncludeStartupTrigger
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

function Test-IsAdministrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Resolve-Mt5TerminalPath {
    param(
        [string]$InputPath
    )

    $attempted = New-Object System.Collections.Generic.List[string]

    if (-not [string]::IsNullOrWhiteSpace($InputPath)) {
        $attempted.Add($InputPath) | Out-Null
        if (Test-Path -LiteralPath $InputPath) {
            $resolved = (Resolve-Path -LiteralPath $InputPath).Path
            return [PSCustomObject]@{
                Path      = $resolved
                Attempted = $attempted.ToArray()
            }
        }
    }

    $candidatePaths = @()
    if ($env:ProgramFiles) {
        $candidatePaths += (Join-Path $env:ProgramFiles "MetaTrader 5\terminal64.exe")
    }
    if ($env:LOCALAPPDATA) {
        $candidatePaths += (Join-Path $env:LOCALAPPDATA "Programs\MetaTrader 5\terminal64.exe")
    }

    if ($env:ProgramFiles) {
        $pfMetaTraderDirs = Get-ChildItem -LiteralPath $env:ProgramFiles -Directory -ErrorAction SilentlyContinue |
            Where-Object { $_.Name -match "MetaTrader|MT5" }

        foreach ($dir in $pfMetaTraderDirs) {
            $candidatePaths += (Join-Path $dir.FullName "terminal64.exe")
        }
    }

    if (${env:ProgramFiles(x86)}) {
        $pf86MetaTraderDirs = Get-ChildItem -LiteralPath ${env:ProgramFiles(x86)} -Directory -ErrorAction SilentlyContinue |
            Where-Object { $_.Name -match "MetaTrader|MT5" }

        foreach ($dir in $pf86MetaTraderDirs) {
            $candidatePaths += (Join-Path $dir.FullName "terminal64.exe")
        }
    }

    $uniqueCandidates = $candidatePaths | Where-Object { -not [string]::IsNullOrWhiteSpace($_) } | Select-Object -Unique
    foreach ($candidate in $uniqueCandidates) {
        $attempted.Add($candidate) | Out-Null
        if (Test-Path -LiteralPath $candidate) {
            $resolved = (Resolve-Path -LiteralPath $candidate).Path
            return [PSCustomObject]@{
                Path      = $resolved
                Attempted = $attempted.ToArray()
            }
        }
    }

    return [PSCustomObject]@{
        Path      = $null
        Attempted = $attempted.ToArray()
    }
}

if (-not (Test-IsAdministrator)) {
    throw "Run PowerShell as Administrator and retry."
}

if ([string]::IsNullOrWhiteSpace($RunAsUser)) {
    if ([string]::IsNullOrWhiteSpace($env:USERDOMAIN)) {
        $RunAsUser = "$env:COMPUTERNAME\$env:USERNAME"
    } else {
        $RunAsUser = "$env:USERDOMAIN\$env:USERNAME"
    }
}

$resolvedTerminal = Resolve-Mt5TerminalPath -InputPath $TerminalPath
if ([string]::IsNullOrWhiteSpace($resolvedTerminal.Path)) {
    $attemptedText = ($resolvedTerminal.Attempted | ForEach-Object { " - $_" }) -join [Environment]::NewLine
    if ([string]::IsNullOrWhiteSpace($attemptedText)) {
        $attemptedText = " - (no candidate path generated)"
    }
    throw "terminal64.exe was not found. Provide -TerminalPath explicitly.`nTried:`n$attemptedText"
}

$launcherScript = Join-Path -Path $PSScriptRoot -ChildPath "start_mt5_single_instance.ps1"
if (-not (Test-Path -LiteralPath $launcherScript)) {
    throw "Missing launcher script: $launcherScript"
}

$escapedLauncherScript = $launcherScript -replace '"', '""'
$escapedTerminalPath = $resolvedTerminal.Path -replace '"', '""'
$launcherArgs = "-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File `"$escapedLauncherScript`" -TerminalPath `"$escapedTerminalPath`""
if (-not [string]::IsNullOrWhiteSpace($TerminalArguments)) {
    $escapedTerminalArguments = $TerminalArguments -replace '"', '""'
    $launcherArgs += " -TerminalArguments `"$escapedTerminalArguments`""
}
$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument $launcherArgs

$triggers = New-Object System.Collections.Generic.List[object]
$logonTrigger = New-ScheduledTaskTrigger -AtLogOn -User $RunAsUser
$logonTrigger.Delay = "PT60S"
$triggers.Add($logonTrigger) | Out-Null

if ($IncludeStartupTrigger) {
    $startupTrigger = New-ScheduledTaskTrigger -AtStartup
    $startupTrigger.Delay = "PT60S"
    $triggers.Add($startupTrigger) | Out-Null
}

$settings = New-ScheduledTaskSettingsSet `
    -StartWhenAvailable `
    -RestartCount 10 `
    -RestartInterval (New-TimeSpan -Minutes 1) `
    -MultipleInstances IgnoreNew `
    -AllowStartIfOnBatteries `
    -DontStopIfGoingOnBatteries `
    -WakeToRun `
    -ExecutionTimeLimit (New-TimeSpan -Days 3650)

$principal = New-ScheduledTaskPrincipal `
    -UserId $RunAsUser `
    -LogonType Interactive `
    -RunLevel Highest

$description = "Start MT5 at logon with 60s delay, retry(1m x10), and single-instance policy."

try {
    Register-ScheduledTask `
        -TaskName $TaskName `
        -Description $description `
        -Action $action `
        -Trigger $triggers.ToArray() `
        -Settings $settings `
        -Principal $principal `
        -Force `
        -ErrorAction Stop | Out-Null
} catch {
    throw "Failed to register scheduled task '$TaskName': $($_.Exception.Message)"
}

$taskInfo = $null
try {
    $taskInfo = Get-ScheduledTask -TaskName $TaskName -ErrorAction Stop | Get-ScheduledTaskInfo -ErrorAction Stop
} catch {
    Write-Warning "Task was registered, but task info could not be retrieved: $($_.Exception.Message)"
}

Write-Host ""
Write-Host "Registered task: $TaskName" -ForegroundColor Green
Write-Host "User: $RunAsUser (Interactive logon only)" -ForegroundColor Cyan
Write-Host "MT5: $($resolvedTerminal.Path)" -ForegroundColor Cyan
if (-not [string]::IsNullOrWhiteSpace($TerminalArguments)) {
    Write-Host "Arguments: $TerminalArguments" -ForegroundColor Cyan
}
Write-Host "Triggers: Logon (delay 60s)" -ForegroundColor Cyan
if ($IncludeStartupTrigger) {
    Write-Host "          Startup (delay 60s)" -ForegroundColor Cyan
}
Write-Host "Retry: every 1 minute, up to 10 times" -ForegroundColor Cyan
Write-Host "Multiple instances: IgnoreNew" -ForegroundColor Cyan
if ($taskInfo) {
    Write-Host "LastTaskResult: $($taskInfo.LastTaskResult)" -ForegroundColor Cyan
    Write-Host "NextRunTime: $($taskInfo.NextRunTime)" -ForegroundColor Cyan
}
Write-Host ""
Write-Host "Run now: Start-ScheduledTask -TaskName `"$TaskName`"" -ForegroundColor Yellow
