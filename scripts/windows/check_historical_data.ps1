# check_historical_data.ps1 — PHASE 1 SCAFFOLD (fail-fast price-data guard)
# Handbook §6: the deterministic experiment stalled because data\historical was
# empty and code silently continued. This guard makes that condition FAIL FAST at
# startup instead of running experiments against missing/override CSVs.
#
# Exit 0 = all required <SYMBOL>_M1.csv present (and, if -CheckFresh, recent enough).
# Exit 1 = one or more missing/stale -> caller must NOT start the daemon.
#
# Called by run.ps1 before launching Brain, and reusable from any service wrapper.
# Does NOT start anything.

[CmdletBinding()]
param(
    # Canonical data_id stems expected by school-backtest-v2.lisp:56 (`<SYMBOL>_M1.csv`).
    [string[]]$Symbols = @('USDJPY', 'EURUSD', 'GBPUSD'),
    [switch]$CheckFresh,
    [int]$MaxAgeHours = 48,
    [switch]$Quiet
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\load_env.ps1"

$histDir = Join-Path $env:SWIMMY_HOME 'data\historical'
$missing = New-Object System.Collections.Generic.List[string]
$stale   = New-Object System.Collections.Generic.List[string]

foreach ($sym in $Symbols) {
    $csv = Join-Path $histDir ("{0}_M1.csv" -f $sym)
    if (-not (Test-Path -LiteralPath $csv)) {
        $missing.Add($sym) | Out-Null
        continue
    }
    if ($CheckFresh) {
        $ageH = ((Get-Date) - (Get-Item -LiteralPath $csv).LastWriteTime).TotalHours
        if ($ageH -gt $MaxAgeHours) { $stale.Add(("{0}({1:N0}h)" -f $sym, $ageH)) | Out-Null }
    }
}

# Guard against the override footgun (config.lisp:135 / SWIMMY_BACKTEST_CSV_OVERRIDE):
# per-symbol CSVs are the source of truth; a set override means all symbols score on one CSV.
$overrideWarn = -not [string]::IsNullOrWhiteSpace($env:SWIMMY_BACKTEST_CSV_OVERRIDE)

if ($missing.Count -gt 0 -or $stale.Count -gt 0) {
    $parts = @()
    if ($missing.Count) { $parts += "missing: $($missing -join ',')" }
    if ($stale.Count)   { $parts += "stale: $($stale -join ',')" }
    $msg = "historical CSV guard FAILED [$histDir] -> " + ($parts -join ' | ')
    if (-not $Quiet) { Write-Host $msg -ForegroundColor Red }
    # Best-effort alert (handbook §4.1 notify_failure.ps1 — created in Phase 2).
    $notify = Join-Path $PSScriptRoot 'notify_failure.ps1'
    if (Test-Path -LiteralPath $notify) { & $notify 1 $msg }
    exit 1
}

if ($overrideWarn -and -not $Quiet) {
    Write-Host "WARN: SWIMMY_BACKTEST_CSV_OVERRIDE is set -> all symbols will score on one CSV." -ForegroundColor Yellow
}
if (-not $Quiet) { Write-Host "historical CSV guard OK ($($Symbols -join ',')) in $histDir" -ForegroundColor Green }
exit 0
