param(
    [string]$PythonExe = "",
    [string]$RepoRoot = "",
    [int]$CycleMinutes = 15,
    [string]$TaskPrefix = "Swimmy-XAU",
    [switch]$EnableLive,
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

$cycleTask = "$TaskPrefix-Cycle"
$execTask = "$TaskPrefix-Exec"
$startupLauncher = Join-Path (Join-Path $env:APPDATA "Microsoft\\Windows\\Start Menu\\Programs\\Startup") "$execTask.cmd"

$cycleCmd = "powershell -NoProfile -ExecutionPolicy Bypass -File `"$runAll`" -PythonExe `"$PythonExe`""
if ($SkipProbe) {
    $cycleCmd += " -SkipProbe"
}

$execCmd = "powershell -NoProfile -ExecutionPolicy Bypass -File `"$liveLoop`" -PythonExe `"$PythonExe`" -PollSeconds 10"
if ($EnableLive) {
    $execCmd += " -Live"
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
    if ($DryRun) {
        Write-Host "DRYRUN remove startup launcher: $startupLauncher"
    } elseif (Test-Path $startupLauncher) {
        Remove-Item -Path $startupLauncher -Force
    }
    Write-Host "Removed tasks: $cycleTask, $execTask"
    exit 0
}

if ($CycleMinutes -lt 1) {
    throw "CycleMinutes must be >= 1"
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

if ($RunNow) {
    Invoke-Schtasks @("/Run", "/TN", $cycleTask) | Out-Null
    if ($code -eq 0) {
        Invoke-Schtasks @("/Run", "/TN", $execTask) | Out-Null
    }
}

Write-Host "Configured tasks:"
Write-Host "  $cycleTask => $cycleCmd"
if ($code -eq 0) {
    Write-Host "  $execTask  => $execCmd"
} else {
    Write-Host "  $execTask  => startup launcher $startupLauncher"
}
