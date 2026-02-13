param(
    [string]$PythonExe = "python",
    [string]$RepoRoot = "",
    [string]$Symbol = "XAUUSD",
    [int]$Magic = 560070,
    [string]$CommentPrefix = "xau_autobot_tuned_auto",
    [int]$Days = 30,
    [switch]$IncludeDetails,
    [string]$WriteReport = ""
)

$ErrorActionPreference = "Stop"
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($RepoRoot)) {
    $RepoRoot = Resolve-Path (Join-Path $scriptDir "..\..")
}
Set-Location $RepoRoot

if ([string]::IsNullOrWhiteSpace($WriteReport)) {
    $stamp = (Get-Date).ToUniversalTime().ToString("yyyyMMdd_HHmmss")
    $WriteReport = "data/reports/xau_autobot_live_report_${stamp}_${Days}d.json"
}

$args = @(
    "tools/xau_autobot_live_report.py",
    "--symbol", $Symbol,
    "--magic", $Magic.ToString(),
    "--comment-prefix", $CommentPrefix,
    "--days", $Days.ToString(),
    "--write-report", $WriteReport
)
if ($IncludeDetails) {
    $args += "--include-details"
}

& $PythonExe @args
exit $LASTEXITCODE
