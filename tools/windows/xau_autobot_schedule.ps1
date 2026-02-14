param(
    [string]$PythonExe = "",
    [string]$RepoRoot = "",
    [int]$CycleMinutes = 15,
    [string]$TaskPrefix = "Swimmy-XAU",
    [switch]$ShowWindow,
    [switch]$EnableLive,
    [switch]$EnableDailyReport,
    [string]$DailyReportTime = "06:10",
    [int]$ReportDays = 30,
    [switch]$ReportIncludeDetails,
    [switch]$ReportDiagnostics,
    [int]$ReportNotifyThresholdClosed = 0,
    [string]$ReportNotifyWebhook = "",
    [string]$ReportNotifyStatePath = "",
    [switch]$SkipProbe,
    [switch]$RunNow,
    [switch]$Remove,
    [switch]$DryRun
)

$ErrorActionPreference = "Stop"
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($RepoRoot)) {
    $RepoRoot = Resolve-Path (Join-Path $scriptDir "..\..")
}

if ([string]::IsNullOrWhiteSpace($PythonExe)) {
    try {
        $PythonExe = (& py -3 -c "import sys; print(sys.executable)").Trim()
    } catch {
        $PythonExe = "python"
    }
}

$runAll = Join-Path $scriptDir "xau_autobot_run_all.ps1"
$liveLoop = Join-Path $scriptDir "xau_autobot_live_loop.ps1"
$liveReport = Join-Path $scriptDir "xau_autobot_live_report.ps1"
$runHidden = Join-Path $scriptDir "run_ps1_hidden.vbs"

$cycleTask = "$TaskPrefix-Cycle"
$execTask = "$TaskPrefix-Exec"
$reportTask = "$TaskPrefix-Report"
$startupLauncherCmd = Join-Path (Join-Path $env:APPDATA "Microsoft\\Windows\\Start Menu\\Programs\\Startup") "$execTask.cmd"
$startupLauncherVbs = Join-Path (Join-Path $env:APPDATA "Microsoft\\Windows\\Start Menu\\Programs\\Startup") "$execTask.vbs"

$cycleArgs = @($runAll, "-PythonExe", $PythonExe)
if ($SkipProbe) { $cycleArgs += "-SkipProbe" }

$execArgs = @($liveLoop, "-PythonExe", $PythonExe, "-PollSeconds", "10")
if ($EnableLive) { $execArgs += "-Live" }

function Quote-TaskArg {
    param([string]$Arg)
    # /TR is a single string; quote each arg for Task Scheduler command-line parsing.
    # We don't expect embedded quotes in our args, but escape them defensively.
    $escaped = $Arg -replace '"', '\\"'
    return '"' + $escaped + '"'
}

function Build-TaskCommand {
    param([string[]]$RunArgs)
    if ($ShowWindow) {
        # Legacy behavior: visible PowerShell window.
        $scriptPath = $RunArgs[0]
        $rest = @()
        if ($RunArgs.Length -gt 1) { $rest = $RunArgs[1..($RunArgs.Length - 1)] }

        $cmdArgs = @("powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", $scriptPath) + $rest
        return ($cmdArgs | ForEach-Object { Quote-TaskArg $_ }) -join " "
    }

    if (-not (Test-Path -LiteralPath $runHidden)) {
        throw "Missing helper script: $runHidden"
    }

    # Use Windows Script Host wrapper so no terminal window pops up (Windows 11 default terminal included).
    $cmdArgs = @("wscript.exe", $runHidden) + $RunArgs
    return ($cmdArgs | ForEach-Object { Quote-TaskArg $_ }) -join " "
}

$cycleCmd = Build-TaskCommand -RunArgs $cycleArgs
$execCmd = Build-TaskCommand -RunArgs $execArgs

$reportArgs = @($liveReport, "-PythonExe", $PythonExe, "-Days", $ReportDays.ToString())
if ($ReportIncludeDetails) { $reportArgs += "-IncludeDetails" }
if ($ReportDiagnostics) { $reportArgs += "-Diagnostics" }
if ($ReportNotifyThresholdClosed -gt 0) { $reportArgs += @("-NotifyThresholdClosed", $ReportNotifyThresholdClosed.ToString()) }
if (-not [string]::IsNullOrWhiteSpace($ReportNotifyWebhook)) { $reportArgs += @("-NotifyWebhook", $ReportNotifyWebhook) }
if (-not [string]::IsNullOrWhiteSpace($ReportNotifyStatePath)) { $reportArgs += @("-NotifyStatePath", $ReportNotifyStatePath) }
$reportCmd = Build-TaskCommand -RunArgs $reportArgs

function Install-StartupLauncher {
    param([string]$CommandLine)
    if ($ShowWindow) {
        $launcherDir = Split-Path -Parent $startupLauncherCmd
        if (-not (Test-Path $launcherDir)) {
            New-Item -Path $launcherDir -ItemType Directory -Force | Out-Null
        }
        $content = "@echo off`r`n$CommandLine`r`n"
        Set-Content -Path $startupLauncherCmd -Value $content -Encoding Ascii
        return $startupLauncherCmd
    }

    $launcherDir = Split-Path -Parent $startupLauncherVbs
    if (-not (Test-Path $launcherDir)) {
        New-Item -Path $launcherDir -ItemType Directory -Force | Out-Null
    }
    $vbs = @"
Option Explicit
Dim shell
Set shell = CreateObject("WScript.Shell")
shell.Run $([string]::Join("", '"', ($CommandLine -replace '"', '""'), '"')) , 0, False
"@
    Set-Content -Path $startupLauncherVbs -Value $vbs -Encoding Ascii
    return $startupLauncherVbs
}

function Invoke-Schtasks {
    param([string[]]$TaskArgs)
    if ($DryRun) {
        Write-Host ("DRYRUN schtasks " + ($TaskArgs -join " "))
        return 0
    }
    & schtasks.exe @TaskArgs | Out-Host
    return $LASTEXITCODE
}

if ($Remove) {
    Invoke-Schtasks @("/Delete", "/TN", $cycleTask, "/F") | Out-Null
    Invoke-Schtasks @("/Delete", "/TN", $execTask, "/F") | Out-Null
    Invoke-Schtasks @("/Delete", "/TN", $reportTask, "/F") | Out-Null
    if ($DryRun) {
        Write-Host "DRYRUN remove startup launcher: $startupLauncherCmd"
        Write-Host "DRYRUN remove startup launcher: $startupLauncherVbs"
    } else {
        if (Test-Path $startupLauncherCmd) { Remove-Item -Path $startupLauncherCmd -Force }
        if (Test-Path $startupLauncherVbs) { Remove-Item -Path $startupLauncherVbs -Force }
    }
    Write-Host "Removed tasks: $cycleTask, $execTask, $reportTask"
    exit 0
}

if ($CycleMinutes -lt 1) {
    throw "CycleMinutes must be >= 1"
}
if ($ReportDays -lt 1) {
    throw "ReportDays must be >= 1"
}
if ($EnableDailyReport -and ($DailyReportTime -notmatch "^(?:[01]\d|2[0-3]):[0-5]\d$")) {
    throw "DailyReportTime must be HH:mm (24h), e.g. 06:10"
}

$code = Invoke-Schtasks @("/Create", "/F", "/SC", "MINUTE", "/MO", $CycleMinutes.ToString(), "/TN", $cycleTask, "/TR", $cycleCmd)
if ($code -ne 0) { exit $code }

$code = Invoke-Schtasks @("/Create", "/F", "/SC", "ONLOGON", "/TN", $execTask, "/TR", $execCmd)
if ($code -ne 0) {
    if ($DryRun) {
        Write-Host "DRYRUN fallback startup launcher: $startupLauncherVbs"
    } else {
        $launcherPath = Install-StartupLauncher -CommandLine $execCmd
        Write-Host "Exec ONLOGON task denied. Fallback startup launcher installed: $launcherPath"
    }
}

$reportCode = 0
if ($EnableDailyReport) {
    $reportCode = Invoke-Schtasks @("/Create", "/F", "/SC", "DAILY", "/ST", $DailyReportTime, "/TN", $reportTask, "/TR", $reportCmd)
    if ($reportCode -ne 0) {
        Write-Host "Failed to create daily report task: $reportTask"
        exit $reportCode
    }
}

if ($RunNow) {
    Invoke-Schtasks @("/Run", "/TN", $cycleTask) | Out-Null
    if ($code -eq 0) {
        Invoke-Schtasks @("/Run", "/TN", $execTask) | Out-Null
    }
    if ($EnableDailyReport -and ($reportCode -eq 0)) {
        Invoke-Schtasks @("/Run", "/TN", $reportTask) | Out-Null
    }
}

Write-Host "Configured tasks:"
Write-Host "  $cycleTask => $cycleCmd"
if ($code -eq 0) {
    Write-Host "  $execTask  => $execCmd"
} else {
    if ($ShowWindow) {
        Write-Host "  $execTask  => startup launcher $startupLauncherCmd"
    } else {
        Write-Host "  $execTask  => startup launcher $startupLauncherVbs"
    }
}
if ($EnableDailyReport) {
    Write-Host "  $reportTask => $reportCmd (daily at $DailyReportTime local time)"
}
