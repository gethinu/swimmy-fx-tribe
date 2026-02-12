param(
    [switch]$RunOnboard,
    [switch]$SkipProbe,
    [string]$ProbeCommand = "openclaw signals --format jsonl"
)

$ErrorActionPreference = "Stop"

function Backup-FileIfExists {
    param([string]$Path)
    if (-not (Test-Path -LiteralPath $Path)) {
        return $null
    }
    $stamp = Get-Date -Format "yyyyMMdd_HHmmss"
    $backup = "$Path.bak.$stamp"
    Copy-Item -LiteralPath $Path -Destination $backup -Force
    return $backup
}

Write-Host "[1/6] Checking OpenClaw command..."
if (-not (Get-Command openclaw -ErrorAction SilentlyContinue)) {
    throw "openclaw command not found. Install first: iwr -useb https://openclaw.ai/install.ps1 | iex"
}

$home = [Environment]::GetFolderPath("UserProfile")
$legacyDir = Join-Path $home ".clawdbot"
$stateDir = Join-Path $home ".openclaw"
$legacyCfg = Join-Path $legacyDir "clawdbot.json"
$newCfg = Join-Path $stateDir "openclaw.json"

Write-Host "[2/6] Backing up config files (if present)..."
$legacyBackup = Backup-FileIfExists -Path $legacyCfg
$newBackup = Backup-FileIfExists -Path $newCfg
if ($legacyBackup) { Write-Host "  backup: $legacyBackup" }
if ($newBackup) { Write-Host "  backup: $newBackup" }

Write-Host "[3/6] Removing invalid config candidates..."
if (Test-Path -LiteralPath $legacyCfg) { Remove-Item -LiteralPath $legacyCfg -Force }
if (Test-Path -LiteralPath $newCfg) { Remove-Item -LiteralPath $newCfg -Force }

Write-Host "[4/6] Running doctor fix..."
& openclaw doctor --fix

if ($RunOnboard) {
    Write-Host "[5/6] Running onboard..."
    & openclaw onboard
} else {
    Write-Host "[5/6] Skipped onboard (use -RunOnboard to execute)."
}

if (-not $SkipProbe) {
    Write-Host "[6/6] Probing signal command..."
    $output = Invoke-Expression $ProbeCommand
    if ($LASTEXITCODE -ne 0) {
        throw "Probe command failed: $ProbeCommand"
    }
    $preview = ($output | Select-Object -First 3) -join "`n"
    Write-Host "Probe output preview:"
    Write-Host $preview
} else {
    Write-Host "[6/6] Probe skipped."
}

Write-Host ""
Write-Host "Repair complete."
Write-Host "Next: set POLYCLAW_OPENCLAW_CMD to your working signal command and disable heuristic fallback."
