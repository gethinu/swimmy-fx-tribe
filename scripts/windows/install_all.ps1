# install_all.ps1 — PHASE 2: unified native-Windows Task Scheduler installer (dry-run orchestrator).
# Handbook doc/knowledge/ops_box_native_windows_migration_20260703.md §1.2–§1.6, §7 Phase 2.
#
# Integrates the two scaffolds into a single planning/registration entry point:
#   install_tasks.ps1   -> 13 always-on daemons (brain, guardian, data-keeper, notifier, ...)
#                          [AtStartup, Restart=always via RestartCount/RestartInterval]
#   install_timers.ps1  -> 17 timer-driven oneshots (macro, oos-monitor, edge-scorecard, ...)
#                          [interval / daily / boot triggers, catchup-wrapped]
#
# DRY-RUN by default: prints the full plan (30 scheduled tasks) without registering anything.
# -Apply performs the REAL registration and REQUIRES Administrator. That is the production
# deployment step and MUST NOT be run until the Phase 1/2 smoke tests pass and it is approved.
# This script itself never starts live trading and never launches the Brain.

[CmdletBinding()]
param(
    [switch]$Apply,
    [string]$Prefix = 'Swimmy',
    [string]$RunAsUser = ''
)
$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\load_env.ps1"

function Test-IsAdministrator {
    $id = [Security.Principal.WindowsIdentity]::GetCurrent()
    (New-Object Security.Principal.WindowsPrincipal($id)).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

Write-Host '=== SWIMMY native-Windows Task Scheduler install (unified) ===' -ForegroundColor Cyan
$mode = if ($Apply) { 'APPLY — REGISTER SCHEDULED TASKS' } else { 'DRY-RUN — plan only, nothing registered' }
Write-Host ("mode : {0}" -f $mode) -ForegroundColor ($(if ($Apply) { 'Red' } else { 'Yellow' }))
Write-Host ("prefix: {0}" -f $Prefix)

if ($Apply -and -not (Test-IsAdministrator)) {
    throw 'install_all.ps1 -Apply must run as Administrator (Register-ScheduledTask requires elevation).'
}

# Common parameters splatted to both installers. Only forward RunAsUser when set so each
# scaffold keeps its own "$env:USERDOMAIN\$env:USERNAME" default.
$common = @{ Prefix = $Prefix }
if ($Apply)     { $common['Apply'] = $true }
if ($RunAsUser) { $common['RunAsUser'] = $RunAsUser }

Write-Host "`n--- [1/2] always-on daemons (install_tasks.ps1) — startup order: infra -> brain -> guardian ---" -ForegroundColor Cyan
& "$PSScriptRoot\install_tasks.ps1" @common

Write-Host "`n--- [2/2] timer-driven oneshots (install_timers.ps1) ---" -ForegroundColor Cyan
& "$PSScriptRoot\install_timers.ps1" @common

if (-not $Apply) {
    Write-Host "`n=== DRY-RUN complete: 13 daemons + 17 timers planned above (nothing registered). ===" -ForegroundColor Yellow
    Write-Host 'To deploy (production): run in an elevated shell with -Apply — get sign-off first.' -ForegroundColor Yellow
} else {
    Write-Host "`n=== APPLY complete. Verify with: Get-ScheduledTask -TaskPath '\' | ? TaskName -like '$Prefix-*' ===" -ForegroundColor Green
}
