# install_timers.ps1 — PHASE 2 SCAFFOLD (systemd 17 .timer -> Task Scheduler)
# Handbook §1.3 + §1.6. Registers the timer-driven oneshots as scheduled tasks.
# Persistent=true is approximated by -StartWhenAvailable plus the per-task last-run
# "catchup stamp" (see run_oneshot_with_catchup below), so high-frequency tasks that
# were missed during downtime fire once on resume instead of storming.
#
# TEMPLATE + DRY-RUN by default (-Apply to register). SBCL-independent: none of these
# require Brain. Does NOT start live trading. RandomizedDelaySec -> -RandomDelay.

[CmdletBinding()]
param(
    [switch]$Apply,
    [string]$RunAsUser = "",
    [string]$Prefix = "Swimmy"
)
$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\load_env.ps1"
$py  = Join-Path $env:SWIMMY_HOME '.venv\Scripts\python.exe'

if ([string]::IsNullOrWhiteSpace($RunAsUser)) {
    $RunAsUser = if ([string]::IsNullOrWhiteSpace($env:USERDOMAIN)) { "$env:COMPUTERNAME\$env:USERNAME" } else { "$env:USERDOMAIN\$env:USERNAME" }
}

# key            : task suffix
# every / at      : schedule (minutes for interval, or HH:mm daily, or 'hourly'/'daily'/'monthly'/'bootN+everyM')
# rand            : RandomDelay minutes (0 = none)
# exec            : command tail run via the catchup wrapper
$timers = @(
    @{ Name='openclaw-signal-sync';         Every=5;    Rand=0;  Exec='tools\openclaw_signal_sync.py' }
    @{ Name='oos-monitor';                  Every=10;   Rand=0;  Exec='tools\oos_monitor.py' }
    @{ Name='polymarket-openclaw-status';   Every=10;   Rand=0;  Exec='tools\polymarket_openclaw_status.py --fail-on-problem' }
    @{ Name='macro';                        Every=15;   Rand=0;  Exec='tools\download_macro_data.py' }
    @{ Name='polymarket-openclaw';          Every=30;   Rand=0;  Exec='tools\run_polymarket_openclaw_service.py' }
    @{ Name='archive-reconcile';            Hourly=$true; Rand=10; Exec='tools\ops\reconcile_archive_db.py --no-data-sexp --grace-sec 600 --prune-db-only' }
    @{ Name='forward-probe-watch';          Hourly=$true; Rand=5;  Exec='tools\ops\forward_probe_watch_runner.sh' }
    @{ Name='armada-paper-readiness';       Daily='00:00'; Rand=15; Exec='tools\ops\armada_paper_readiness_runner.sh' }
    @{ Name='edge-scorecard';               Daily='00:00'; Rand=10; Exec='tools\edge_scorecard_runner.sh' }
    @{ Name='rank-conformance-audit';       Daily='00:00'; Rand=15; Exec='tools\rank_conformance_audit_runner.sh' }
    @{ Name='polymarket-weather-calibration';Daily='06:05'; Rand=10; Exec='tools\polymarket_weather_calibration_update.py' }
    @{ Name='pattern-backend-calibration';  Daily='06:20'; Rand=15; Exec='tools\pattern_backend_calibration_update.py' }
    @{ Name='system-audit';                 Daily='06:40'; Rand=20; Exec='tools\system_audit.sh' }
    @{ Name='history-update';               Monthly=$true; Rand=0;  Exec='tools\update_history_smart.py'; Wrap='guardian-stop-start' }
    @{ Name='polymarket-weather-snapshot';  BootMin=5;  Every=15; Rand=0; Exec='tools\polymarket_weather_snapshot.py' }
    @{ Name='trend-arbitrage';              BootMin=5;  Every=120; Rand=0; Exec='tools\trend_arbitrage_runner.sh' }
    @{ Name='xau-autobot-cycle';            BootMin=2;  Every=15; Rand=0; Exec='tools\xau_autobot_cycle_runner.sh' }
)

function New-Trigger($t) {
    if ($t.ContainsKey('Hourly'))  { $tr = New-ScheduledTaskTrigger -Once -At (Get-Date).Date -RepetitionInterval (New-TimeSpan -Hours 1); return $tr }
    if ($t.ContainsKey('Daily'))   { return New-ScheduledTaskTrigger -Daily -At $t.Daily }
    if ($t.ContainsKey('Monthly')) { return New-ScheduledTaskTrigger -Once -At (Get-Date).Date }  # + monthly repetition set post-register
    if ($t.ContainsKey('BootMin')) {
        $tr = New-ScheduledTaskTrigger -AtStartup
        $tr.Delay = "PT$($t.BootMin)M"
        $tr.Repetition = (New-ScheduledTaskTrigger -Once -At (Get-Date).Date -RepetitionInterval (New-TimeSpan -Minutes $t.Every)).Repetition
        return $tr
    }
    # plain interval (Every minutes)
    return New-ScheduledTaskTrigger -Once -At (Get-Date).Date -RepetitionInterval (New-TimeSpan -Minutes $t.Every)
}

foreach ($t in $timers) {
    $taskName = "$Prefix-$($t.Name)"
    $intervalMin = if ($t.ContainsKey('Every')) { $t.Every } elseif ($t.ContainsKey('Hourly')) { 60 } else { 0 }
    # Wrap every oneshot with the catchup stamp (§1.6). For .sh execs, Phase 2 will
    # point these at the ported .ps1; shown here as the intended command tail.
    $inner = if ($t.Exec -like '*.py*') { "& `"$py`" $($t.Exec)" } else { "& `"$(Join-Path $env:SWIMMY_HOME $t.Exec)`"  # TODO Phase2: .sh -> .ps1" }
    $wrapNote = if ($t.ContainsKey('Wrap')) { "  [wrap=$($t.Wrap): stop guardian -> run -> start guardian]" } else { "" }

    if (-not $Apply) {
        $sched = if ($t.ContainsKey('Daily')) { "daily@$($t.Daily)" } elseif ($t.ContainsKey('Hourly')) { 'hourly' } elseif ($t.ContainsKey('Monthly')) { 'monthly' } elseif ($t.ContainsKey('BootMin')) { "boot+$($t.BootMin)m/every$($t.Every)m" } else { "every$($t.Every)m" }
        Write-Host ("[plan] {0,-34} {1,-20} rand={2,2}m catchup={3}m{4}" -f $taskName, $sched, $t.Rand, $intervalMin, $wrapNote) -ForegroundColor DarkCyan
        continue
    }
    # --- actual registration (only with -Apply) ---
    $argLine = "-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -Command `"& '$PSScriptRoot\run_oneshot_with_catchup.ps1' -Name '$($t.Name)' -IntervalMin $intervalMin -Command { $inner }`""
    $action  = New-ScheduledTaskAction -Execute 'powershell.exe' -Argument $argLine
    $trigger = New-Trigger $t
    if ($t.Rand -gt 0) { try { $trigger.RandomDelay = "PT$($t.Rand)M" } catch {} }
    $settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -MultipleInstances IgnoreNew -ExecutionTimeLimit (New-TimeSpan -Hours 6)
    $principal = New-ScheduledTaskPrincipal -UserId $RunAsUser -LogonType S4U -RunLevel Highest
    Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Settings $settings -Principal $principal -Force | Out-Null
    Write-Host "[registered] $taskName" -ForegroundColor Green
}

if (-not $Apply) { Write-Host "`nDRY RUN. 17 timers planned. -Apply (as Admin) to register. Persistent=true -> -StartWhenAvailable + catchup stamp (§1.6)." -ForegroundColor Yellow }
