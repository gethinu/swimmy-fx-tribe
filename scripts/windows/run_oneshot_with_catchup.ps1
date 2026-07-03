# run_oneshot_with_catchup.ps1 — PHASE 2 SCAFFOLD (handbook §1.6)
# Wraps a timer-driven oneshot so that Persistent=true is approximated: if the task
# fires having already run within its interval (e.g. Task Scheduler replayed several
# missed -StartWhenAvailable firings after downtime), it self-skips instead of storming.
#
# Records last-run time under %SWIMMY_HOME%\data\runstamps\<Name>.txt.
# TEMPLATE — invoked by install_timers.ps1's registered tasks.

[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$Name,
    [Parameter(Mandatory)][int]$IntervalMin,
    [Parameter(Mandatory)][scriptblock]$Command
)
$ErrorActionPreference = 'Continue'
. "$PSScriptRoot\load_env.ps1"

$stampDir = Join-Path $env:SWIMMY_HOME 'data\runstamps'
if (-not (Test-Path $stampDir)) { New-Item -ItemType Directory -Path $stampDir | Out-Null }
$stamp = Join-Path $stampDir "$Name.txt"

if ($IntervalMin -gt 0 -and (Test-Path $stamp)) {
    $last = try { Get-Content $stamp -Raw | Get-Date } catch { $null }
    if ($last -and ((Get-Date) - $last).TotalMinutes -lt $IntervalMin) {
        Write-Host "[catchup] $Name ran $([int]((Get-Date)-$last).TotalMinutes)m ago (< ${IntervalMin}m) -> skip"
        exit 0
    }
}

$log = Join-Path $env:SWIMMY_HOME "logs\$Name.log"
try {
    & $Command *>> $log
    $code = $LASTEXITCODE
} finally {
    (Get-Date).ToString('o') | Set-Content $stamp
}
exit ($(if ($null -ne $code) { $code } else { 0 }))
