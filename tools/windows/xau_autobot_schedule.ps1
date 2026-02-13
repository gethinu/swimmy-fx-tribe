param(
    [string]$PythonExe = "",
    [string]$RepoRoot = "",
    [int]$CycleMinutes = 15,
    [string]$TaskPrefix = "Swimmy-XAU",
    [switch]$EnableLive,
    [switch]$EnableDailyReport,
    [string]$DailyReportTime = "06:10",
    [int]$ReportDays = 30,
    [switch]$ReportIncludeDetails,
    [switch]$ReportDiagnostics,
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

$cycleTask = "$TaskPrefix-Cycle"
$execTask = "$TaskPrefix-Exec"
$reportTask = "$TaskPrefix-Report"
$startupLauncher = Join-Path (Join-Path $env:APPDATA "Microsoft\\Windows\\Start Menu\\Programs\\Startup") "$execTask.cmd"

$cycleCmd = "powershell -NoProfile -ExecutionPolicy Bypass -File `"$runAll`" -PythonExe `"$PythonExe`""
if ($SkipProbe) {
    $cycleCmd += " -SkipProbe"
}

$execCmd = "powershell -NoProfile -ExecutionPolicy Bypass -File `"$liveLoop`" -PythonExe `"$PythonExe`" -PollSeconds 10"
if ($EnableLive) {
    $execCmd += " -Live"
}

$reportCmd = "powershell -NoProfile -ExecutionPolicy Bypass -File `"$liveReport`" -PythonExe `"$PythonExe`" -Days $ReportDays"
if ($ReportIncludeDetails) {
    $reportCmd += " -IncludeDetails"
}
if ($ReportDiagnostics) {
    $reportCmd += " -Diagnostics"
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

function Install-StartupLauncher {
    param([string]$CommandLine)
    $launcherDir = Split-Path -Parent $startupLauncher
    if (-not (Test-Path $launcherDir)) {
        New-Item -Path $launcherDir -ItemType Directory -Force | Out-Null
    }
    $content = "@echo off`r`n$CommandLine`r`n"
    Set-Content -Path $startupLauncher -Value $content -Encoding Ascii
    return $startupLauncher
}

if ($Remove) {
    Invoke-Schtasks @("/Delete", "/TN", $cycleTask, "/F") | Out-Null
    Invoke-Schtasks @("/Delete", "/TN", $execTask, "/F") | Out-Null
    Invoke-Schtasks @("/Delete", "/TN", $reportTask, "/F") | Out-Null
    if ($DryRun) {
        Write-Host "DRYRUN remove startup launcher: $startupLauncher"
    } elseif (Test-Path $startupLauncher) {
        Remove-Item -Path $startupLauncher -Force
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
        Write-Host "DRYRUN fallback startup launcher: $startupLauncher"
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
    Write-Host "  $execTask  => startup launcher $startupLauncher"
}
if ($EnableDailyReport) {
    Write-Host "  $reportTask => $reportCmd (daily at $DailyReportTime local time)"
}
