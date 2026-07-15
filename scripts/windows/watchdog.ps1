# watchdog.ps1 — PHASE 2 SCAFFOLD (native-Windows port of tools/watchdog.py)
# Handbook §2. Replaces the Linux-only bits of watchdog.py:
#   systemctl show -p MainPID   -> Get-Process by cmdline / Get-ScheduledTaskInfo
#   systemctl restart <svc>     -> Restart-ScheduledTask (== Stop + Start)
#   os.kill(pid, SIGTERM/KILL)  -> Stop-Process (graceful) then -Force
# The SILENCE-TIMER + STARTUP-GRACE logic is unchanged in spirit and reproduced here.
#
# It measures Brain liveness by subscribing to Brain's motor PUB (5556) via the venv
# python (PowerShell has no native ZMQ). On prolonged silence it restarts the Brain task.
#
# TEMPLATE + DRY-RUN by default (-Apply to actually restart). Does NOT register itself,
# does NOT touch live trading. The 3-tier sudo/polkit fallback from watchdog.py is
# intentionally dropped (no elevation concept on a single-user ops box).

[CmdletBinding()]
param(
    [int]$SilenceTimeoutSec = 300,      # watchdog.py DEAD_MAN timeout
    [int]$StartupGraceSec   = 900,      # BRAIN_STARTUP_GRACE_SECS
    [int]$PollSec           = 15,
    [string]$BrainTaskName  = 'Swimmy-brain',
    [switch]$Apply,
    [switch]$Once                        # single evaluation then exit (for testing)
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\load_env.ps1"
$py = Join-Path $env:SWIMMY_HOME '.venv\Scripts\python.exe'
$motorPort = if ($env:SWIMMY_PORT_MOTOR) { $env:SWIMMY_PORT_MOTOR } else { 5556 }
$host_ = if ($env:SWIMMY_ZMQ_HOST) { $env:SWIMMY_ZMQ_HOST } else { 'localhost' }

# Returns $true if any message arrives on Brain's motor PUB within $timeoutMs.
function Test-BrainAlive([int]$timeoutMs = 2000) {
    $probe = @"
import sys, zmq
ctx = zmq.Context()
s = ctx.socket(zmq.SUB); s.connect('tcp://${host_}:${motorPort}'); s.setsockopt(zmq.SUBSCRIBE, b'')
poller = zmq.Poller(); poller.register(s, zmq.POLLIN)
sys.exit(0 if dict(poller.poll($timeoutMs)).get(s) == zmq.POLLIN else 1)
"@
    & $py -c $probe
    return ($LASTEXITCODE -eq 0)
}

function Restart-Brain {
    if (-not $Apply) { Write-Host "[watchdog][DRY-RUN] would restart $BrainTaskName" -ForegroundColor Yellow; return }
    # Prefer the scheduled task's own restart; fall back to killing the sbcl process
    # so the task's RestartCount relaunches it (handbook §2).
    try {
        Restart-ScheduledTask -TaskName $BrainTaskName -ErrorAction Stop
        Write-Host "[watchdog] Restart-ScheduledTask $BrainTaskName"
    } catch {
        Get-CimInstance Win32_Process -Filter "Name='sbcl.exe'" -ErrorAction SilentlyContinue |
            Where-Object { $_.CommandLine -match 'brain\.lisp|swimmy' } |
            ForEach-Object { Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue }
        Write-Host "[watchdog] killed sbcl; task RestartCount should relaunch"
    }
}

# --- silence-timer state (mirror of watchdog.py BrainState) ---
$lastMsg = Get-Date
$graceStart = Get-Date
Write-Host "[watchdog] motor=tcp://$host_`:$motorPort timeout=${SilenceTimeoutSec}s grace=${StartupGraceSec}s apply=$Apply"

do {
    if (Test-BrainAlive) {
        $lastMsg = Get-Date
        Write-Host "[watchdog] brain alive @ $(Get-Date -Format HH:mm:ss)"
    } else {
        $silent = ((Get-Date) - $lastMsg).TotalSeconds
        $inGrace = ((Get-Date) - $graceStart).TotalSeconds -lt $StartupGraceSec
        if ($silent -gt $SilenceTimeoutSec -and -not $inGrace) {
            Write-Host "[watchdog] SILENT ${silent}s > ${SilenceTimeoutSec}s -> restart" -ForegroundColor Red
            Restart-Brain
            $lastMsg = Get-Date; $graceStart = Get-Date    # reset grace after restart (PID-change parity)
        } else {
            Write-Host ("[watchdog] silent {0:N0}s (grace={1})" -f $silent, $inGrace)
        }
    }
    if ($Once) { break }
    Start-Sleep -Seconds $PollSec
} while ($true)
