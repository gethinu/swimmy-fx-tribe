# install_tasks.ps1 — PHASE 1/2 SCAFFOLD (systemd 常駐13本 -> Task Scheduler)
# Handbook: doc/knowledge/ops_box_native_windows_migration_20260703.md §1.2, §7 Phase 2.
# Foundation: scripts/setup_mt5_autostart_task.ps1 (Register-ScheduledTask + RestartCount).
#
# Registers the 13 long-running daemons as -AtStartup tasks with Restart=always
# semantics (RestartCount/RestartInterval). timer-driven oneshots (17) are handled
# separately per §1.3 and are NOT in this file.
#
# THIS IS A TEMPLATE. -WhatIf by default prints the plan without registering.
# Run with -Apply (as Administrator) only after Phase 1 smoke test passes.

[CmdletBinding()]
param(
    [switch]$Apply,
    [string]$RunAsUser = "",
    [string]$Prefix = "Swimmy"   # task names become e.g. "Swimmy-brain"
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. "$PSScriptRoot\load_env.ps1"

function Test-IsAdministrator {
    $id = [Security.Principal.WindowsIdentity]::GetCurrent()
    (New-Object Security.Principal.WindowsPrincipal($id)).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

if ([string]::IsNullOrWhiteSpace($RunAsUser)) {
    $RunAsUser = if ([string]::IsNullOrWhiteSpace($env:USERDOMAIN)) { "$env:COMPUTERNAME\$env:USERNAME" } else { "$env:USERDOMAIN\$env:USERNAME" }
}

$py  = Join-Path $env:SWIMMY_HOME '.venv\Scripts\python.exe'
$run = Join-Path $PSScriptRoot 'run.ps1'

# --- The 13 always-on daemons (handbook §1.2). RestartSec -> RestartInterval. ---
# 'exec' is the command tail; python daemons run via $py, brain/school/guardian via run.ps1 wrappers.
$daemons = @(
    @{ Name = 'brain';             Kind = 'run';    Arg = '-Component brain';                          RestartSec = 10 }
    @{ Name = 'guardian';          Kind = 'run';    Arg = '-Component guardian';                       RestartSec = 5  }
    @{ Name = 'school';            Kind = 'sbcl';   Arg = 'src\lisp\school\school-daemon.lisp';        RestartSec = 10; Pre = 'tools\restore_legend_61.lisp' }
    @{ Name = 'data-keeper';       Kind = 'py';     Arg = 'tools\data_keeper.py';                      RestartSec = 10 }
    @{ Name = 'notifier';          Kind = 'py';     Arg = 'tools\notifier.py';                         RestartSec = 5  }
    @{ Name = 'risk';              Kind = 'py';     Arg = 'tools\risk_gateway.py';                     RestartSec = 5  }
    @{ Name = 'evolution';         Kind = 'py';     Arg = 'tools\evolution_daemon.py';                 RestartSec = 10 }
    @{ Name = 'pattern-similarity';Kind = 'py';     Arg = 'tools\pattern_similarity_service.py';       RestartSec = 10 }
    @{ Name = 'discord-bot';       Kind = 'py';     Arg = 'src\python\discord_bot.py';                 RestartSec = 5  }
    @{ Name = 'mcp-gateway';       Kind = 'py';     Arg = 'tools\mcp_gateway.py';                       RestartSec = 10 }
    @{ Name = 'mt5-account-sync';  Kind = 'py';     Arg = 'tools\mt5_account_sync.py';                 RestartSec = 10; Notify = 'MT5 Account Sync' }
    @{ Name = 'pending-manager';   Kind = 'py';     Arg = 'tools\pending_manager.py';                  RestartSec = 10 }
    @{ Name = 'strategy-hunter';   Kind = 'py';     Arg = 'tools\strategy_hunter.py';                  RestartSec = 10 }
)

function Build-Argument($d) {
    # Every daemon runs through a small wrapper so we get: cd SWIMMY_HOME, load_env, log redirect,
    # notify_failure on non-zero (handbook §1.5). Here we emit the wrapper invocation.
    $wrapper = Join-Path $PSScriptRoot 'svc\run_wrapper.ps1'   # created in Phase 2 from _wrapper_template
    $inner = switch ($d.Kind) {
        'run'  { "& `"$run`" $($d.Arg)" }
        'py'   { "& `"$py`" `"$(Join-Path $env:SWIMMY_HOME $d.Arg)`"" }
        'sbcl' {
            $pre = if ($d.ContainsKey('Pre')) { "sbcl --disable-debugger --script `"$(Join-Path $env:SWIMMY_HOME $d.Pre)`"; " } else { "" }
            "$pre sbcl --dynamic-space-size `$env:SWIMMY_SBCL_DYNAMIC_SPACE_MB --noinform --load `"$(Join-Path $env:SWIMMY_HOME $d.Arg)`""
        }
    }
    # For the scaffold we point directly at run.ps1/python; the §1.5 wrapper wiring is a Phase 2 TODO.
    "-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -Command `"$inner`""
}

$settings = New-ScheduledTaskSettingsSet `
    -StartWhenAvailable `
    -RestartCount 999 `
    -RestartInterval (New-TimeSpan -Seconds 10) `
    -MultipleInstances IgnoreNew `
    -AllowStartIfOnBatteries `
    -DontStopIfGoingOnBatteries `
    -ExecutionTimeLimit ([TimeSpan]::Zero)   # 0 == no limit (long-running daemon)

$principal = New-ScheduledTaskPrincipal -UserId $RunAsUser -LogonType S4U -RunLevel Highest

foreach ($d in $daemons) {
    $taskName = "$Prefix-$($d.Name)"
    $argLine  = Build-Argument $d
    $action   = New-ScheduledTaskAction -Execute 'powershell.exe' -Argument $argLine

    $startup  = New-ScheduledTaskTrigger -AtStartup
    $startup.Delay = 'PT30S'

    # Per-daemon RestartInterval (RestartSec parity) — clone base settings and override.
    $s = $settings.PSObject.Copy()
    $s.RestartInterval = (New-TimeSpan -Seconds $d.RestartSec)

    if (-not $Apply) {
        Write-Host ("[plan] {0,-28} restart={1,2}s  {2}" -f $taskName, $d.RestartSec, $argLine) -ForegroundColor DarkCyan
        continue
    }

    if (-not (Test-IsAdministrator)) { throw 'Run as Administrator to -Apply.' }
    Register-ScheduledTask -TaskName $taskName -Description "Swimmy daemon: $($d.Name) (Restart=always ~$($d.RestartSec)s)" `
        -Action $action -Trigger $startup -Settings $s -Principal $principal -Force -ErrorAction Stop | Out-Null
    Write-Host "[registered] $taskName" -ForegroundColor Green
}

if (-not $Apply) {
    Write-Host ""
    Write-Host "DRY RUN. 13 daemons planned above. Re-run as Administrator with -Apply to register." -ForegroundColor Yellow
    Write-Host "timer-driven oneshots (17) are NOT here — see handbook §1.3." -ForegroundColor Yellow
}
