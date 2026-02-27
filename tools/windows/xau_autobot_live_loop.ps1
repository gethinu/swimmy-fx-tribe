param(
    [string]$PythonExe = "python",
    [string]$RepoRoot = "",
    [string]$ConfigPath = "tools/configs/xau_autobot.tuned_auto_active.json",
    [int]$PollSeconds = 10,
    [string]$InstanceLockPath = "",
    [switch]$Live,
    [switch]$Once
)

$ErrorActionPreference = "Stop"
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($RepoRoot)) {
    $RepoRoot = Resolve-Path (Join-Path $scriptDir "..\..")
}
Set-Location $RepoRoot
if ([string]::IsNullOrWhiteSpace($env:XAU_AUTOBOT_RUNTIME_JOURNAL_PATH)) {
    $env:XAU_AUTOBOT_RUNTIME_JOURNAL_PATH = Join-Path $RepoRoot "data/reports/xau_autobot_runtime_journal_latest.jsonl"
}

$lockDir = Join-Path $RepoRoot "data/runtime/xau_autobot_live_loop_locks"
if ([string]::IsNullOrWhiteSpace($InstanceLockPath)) {
    $configName = [System.IO.Path]::GetFileNameWithoutExtension($ConfigPath)
    if ([string]::IsNullOrWhiteSpace($configName)) {
        $configName = "default"
    }
    $InstanceLockPath = Join-Path $lockDir ($configName + ".lock")
}
$lockStream = $null
try {
    New-Item -ItemType Directory -Path $lockDir -Force | Out-Null
    $lockStream = [System.IO.File]::Open(
        $InstanceLockPath,
        [System.IO.FileMode]::OpenOrCreate,
        [System.IO.FileAccess]::ReadWrite,
        [System.IO.FileShare]::None
    )
} catch {
    $payload = @{
        action = "INFO"
        reason = "live_loop_instance_already_running"
        config_path = $ConfigPath
        lock_path = $InstanceLockPath
    }
    Write-Output ($payload | ConvertTo-Json -Compress)
    exit 0
}

$lockPayload = (@{
    pid = $PID
    started_at_utc = (Get-Date).ToUniversalTime().ToString("o")
    config_path = $ConfigPath
} | ConvertTo-Json -Compress)
$lockBytes = [System.Text.Encoding]::UTF8.GetBytes($lockPayload)
$lockStream.SetLength(0)
$lockStream.Write($lockBytes, 0, $lockBytes.Length)
$lockStream.Flush()

$args = @("tools/xau_autobot.py", "--config", $ConfigPath, "--poll-seconds", $PollSeconds.ToString())
if (-not $Once) {
    $args += "--loop"
}
if ($Live) {
    $args += "--live"
}

$exitCode = 1
try {
    & $PythonExe @args
    $exitCode = $LASTEXITCODE
} finally {
    if ($null -ne $lockStream) {
        $lockStream.Dispose()
    }
    Remove-Item -LiteralPath $InstanceLockPath -Force -ErrorAction SilentlyContinue
}

exit $exitCode
