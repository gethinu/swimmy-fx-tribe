param(
    [string]$PythonExe = "python",
    [string]$RepoRoot = "",
    [string]$ConfigPath = "tools/configs/xau_autobot.tuned_auto_active.json",
    [int]$PollSeconds = 10,
    [switch]$Live,
    [switch]$Once
)

$ErrorActionPreference = "Stop"
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($RepoRoot)) {
    $RepoRoot = Resolve-Path (Join-Path $scriptDir "..\..")
}
Set-Location $RepoRoot

$args = @("tools/xau_autobot.py", "--config", $ConfigPath, "--poll-seconds", $PollSeconds.ToString())
if (-not $Once) {
    $args += "--loop"
}
if ($Live) {
    $args += "--live"
}

& $PythonExe @args
exit $LASTEXITCODE
