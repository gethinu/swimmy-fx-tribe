param(
    [string]$PythonExe = "python",
    [string]$RepoRoot = "",
    [string]$Period = "90d",
    [string]$Interval = "5m",
    [double]$CostPerSide = 0.0002,
    [double]$AssumedCostSide = 0.0002,
    [double]$SpreadPoints = 80,
    [double]$CommissionRoundtripPct = 0.02,
    [double]$SlippageRoundtripPct = 0.01,
    [double]$Point = 0.01,
    [string]$Symbol = "XAUUSD",
    [int]$ProbeSamples = 120,
    [int]$ProbeIntervalMs = 500,
    [switch]$SkipProbe
)

$ErrorActionPreference = "Stop"
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($RepoRoot)) {
    $RepoRoot = Resolve-Path (Join-Path $scriptDir "..\..")
}
Set-Location $RepoRoot

$reportsDir = Join-Path $RepoRoot "data\reports"
New-Item -Path $reportsDir -ItemType Directory -Force | Out-Null

$periodTag = ($Period -replace "[^A-Za-z0-9._-]", "_")
$configPath = "tools/configs/xau_autobot.tuned_auto_gc_m5_${periodTag}.json"
$summaryPath = "data/reports/xau_autobot_cycle_summary_${periodTag}.json"
$readinessPath = "data/reports/xau_autobot_readiness_${periodTag}.json"
$probePath = "data/reports/xau_autobot_windows_probe_${periodTag}.json"

& $PythonExe "tools/xau_autobot_cycle.py" `
    --python-exe $PythonExe `
    --period $Period `
    --interval $Interval `
    --cost-per-side $CostPerSide `
    --assumed-cost-side $AssumedCostSide `
    --spread-points $SpreadPoints `
    --commission-roundtrip-pct $CommissionRoundtripPct `
    --slippage-roundtrip-pct $SlippageRoundtripPct `
    --point $Point `
    --write-config $configPath `
    --write-summary $summaryPath
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

if (-not $SkipProbe) {
    & $PythonExe "tools/xau_autobot_windows_probe.py" `
        --readiness-report $readinessPath `
        --symbol $Symbol `
        --samples $ProbeSamples `
        --interval-ms $ProbeIntervalMs `
        --commission-roundtrip-pct $CommissionRoundtripPct `
        --slippage-roundtrip-pct $SlippageRoundtripPct `
        --write-report $probePath
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

Write-Host "XAU AutoBot run complete"
Write-Host "Summary: $summaryPath"
if (-not $SkipProbe) {
    Write-Host "Probe:   $probePath"
}
