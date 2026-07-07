# run.ps1 — PHASE 1 SCAFFOLD (native Windows port of run.sh)
# Handbook: doc/knowledge/ops_box_native_windows_migration_20260703.md §4.1, §7 Phase 1.
#
# Faithful port of run.sh with the bash/Linux dependencies removed:
#   mktemp        -> New-TemporaryFile
#   date +FMT     -> Get-Date -Format
#   ls|tail|xargs -> Get-ChildItem|Sort|Select -Skip|Remove-Item
#   source .env   -> load_env.ps1
#   | tee         -> Tee-Object
# All paths are %SWIMMY_HOME%-relative. No /home/swimmy, no /tmp.
#
# -Component brain    (default) : launch SBCL Brain only  (== run.sh, one Task Scheduler task)
# -Component guardian           : launch Rust guardian.exe only
# -Component core               : launch Brain + Guardian + data_keeper for the
#                                 Phase 1 GO/NO-GO TCP ZMQ smoke test (foreground, Ctrl-C to stop)
#
# THIS IS A TEMPLATE. Do not wire to Task Scheduler until Phase 1 smoke test passes.

[CmdletBinding()]
param(
    [ValidateSet('brain', 'guardian', 'core')]
    [string]$Component = 'brain',
    [switch]$SkipDataGuard,
    [switch]$DryRun            # -Component core: print the ordered startup plan without launching
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\load_env.ps1"
Set-Location $env:SWIMMY_HOME

$logDir = Join-Path $env:SWIMMY_HOME 'logs'
if (-not (Test-Path -LiteralPath $logDir)) { New-Item -ItemType Directory -Path $logDir | Out-Null }

# --- fail-fast price-data guard (handbook §6) ---
if (-not $SkipDataGuard) {
    & "$PSScriptRoot\check_historical_data.ps1"
    if ($LASTEXITCODE -ne 0) { throw "run.ps1 aborted: historical data guard failed." }
}

function Start-Brain {
    # Log rotation parity with run.sh:24-30
    $mainLog = Join-Path $logDir 'swimmy.log'
    if (Test-Path -LiteralPath $mainLog) {
        $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'
        Move-Item -LiteralPath $mainLog -Destination (Join-Path $logDir "swimmy.$stamp.log")
    }
    # Keep only last 7 rotated logs (run.sh:30)
    Get-ChildItem -LiteralPath $logDir -Filter 'swimmy.*.log' -ErrorAction SilentlyContinue |
        Sort-Object LastWriteTime -Descending | Select-Object -Skip 7 | Remove-Item -Force -ErrorAction SilentlyContinue

    # Bootstrap entrypoint (run.sh:33-65) — prefer brain.lisp, else temp ASDF bootstrap.
    $bootFile = Join-Path $env:SWIMMY_HOME 'brain.lisp'
    $bootTemp = $null
    if (-not (Test-Path -LiteralPath $bootFile)) {
        Write-Warning 'brain.lisp not found; falling back to ASDF bootstrap.'
        $bootTemp = New-TemporaryFile
        @'
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
(format t "~%[LOADER] Loading Swimmy System via ASDF...~%")
(handler-case
    (handler-bind ((style-warning #'muffle-warning) (warning #'muffle-warning))
      (asdf:load-system :swimmy))
  (error (c) (format t "~%[FATAL] Failed to load system: ~a~%" c) (sb-ext:exit :code 1)))
(uiop:symbol-call :swimmy.main :start-system)
'@ | Set-Content -LiteralPath $bootTemp -Encoding UTF8
        $bootFile = $bootTemp.FullName
    }

    $heap = if ($env:SWIMMY_SBCL_DYNAMIC_SPACE_MB) { $env:SWIMMY_SBCL_DYNAMIC_SPACE_MB } else { '6144' }
    $sbcl = Resolve-Sbcl
    Write-Host "[$(Get-Date)] Starting Swimmy Ver 41.5 (heap ${heap}MB) via $sbcl..."
    try {
        # SBCL writes compile notes to stderr; do NOT let that abort the pipeline.
        $ErrorActionPreference = 'Continue'
        if (Get-Variable PSNativeCommandUseErrorActionPreference -Scope Global -ErrorAction SilentlyContinue) {
            $PSNativeCommandUseErrorActionPreference = $false
        }
        # sbcl ... 2>&1 | tee logs/swimmy.log  (run.sh:71)
        & $sbcl --dynamic-space-size $heap --noinform --load $bootFile 2>&1 | Tee-Object -FilePath $mainLog
    } finally {
        if ($bootTemp) { Remove-Item -LiteralPath $bootTemp -Force -ErrorAction SilentlyContinue }
    }
}

function Resolve-Sbcl {
    # winget's SBCL MSI installs to Program Files but does not always add it to PATH.
    $c = Get-Command sbcl -ErrorAction SilentlyContinue
    if ($c) { return $c.Source }
    foreach ($p in @(
        'C:\Program Files\Steel Bank Common Lisp\sbcl.exe',
        'C:\Program Files (x86)\Steel Bank Common Lisp\sbcl.exe',
        (Join-Path $env:LOCALAPPDATA 'Programs\Steel Bank Common Lisp\sbcl.exe')
    )) { if (Test-Path -LiteralPath $p) { return $p } }
    throw "sbcl.exe not found on PATH or standard install dirs. Install SBCL or add it to PATH."
}

function Start-Guardian {
    # Workspace build lands the exe at the repo-root target\, not guardian\target\.
    $exe = Join-Path $env:SWIMMY_HOME 'target\release\guardian.exe'
    if (-not (Test-Path -LiteralPath $exe)) {
        $exe = Join-Path $env:SWIMMY_HOME 'guardian\target\release\guardian.exe'
    }
    if (-not (Test-Path -LiteralPath $exe)) {
        throw "guardian.exe not found (checked target\ and guardian\target\) — run 'cargo build --release' first."
    }
    # ExecStartPre parity: free port 5557 if a stale holder exists (handbook §1.4, was `fuser -k 5557/tcp`).
    try {
        Get-NetTCPConnection -LocalPort 5557 -State Listen -ErrorAction SilentlyContinue |
            ForEach-Object { Stop-Process -Id $_.OwningProcess -Force -ErrorAction SilentlyContinue }
    } catch { }
    $gLog = Join-Path $logDir 'guardian.log'
    Write-Host "[$(Get-Date)] Starting Guardian..."
    $ErrorActionPreference = 'Continue'
    if (Get-Variable PSNativeCommandUseErrorActionPreference -Scope Global -ErrorAction SilentlyContinue) {
        $PSNativeCommandUseErrorActionPreference = $false
    }
    & $exe 2>&1 | Tee-Object -FilePath $gLog
}

# Poll until every port in $Ports is LISTEN, or $TimeoutSec elapses. Readiness gate
# for the ordered core startup so the next component only starts once its dependency
# is actually bound (replaces a blind Start-Sleep).
function Wait-Ports {
    param([int[]]$Ports, [int]$TimeoutSec = 120, [string]$Name = 'component')
    $deadline = (Get-Date).AddSeconds($TimeoutSec)
    do {
        $up = @($Ports | Where-Object { Get-NetTCPConnection -LocalPort $_ -State Listen -ErrorAction SilentlyContinue })
        if ($up.Count -eq $Ports.Count) {
            Write-Host ("  [{0}] READY: {1} LISTEN" -f $Name, ($Ports -join ',')) -ForegroundColor Green
            return $true
        }
        Start-Sleep -Seconds 3
    } until ((Get-Date) -gt $deadline)
    Write-Host ("  [{0}] TIMEOUT after {1}s waiting for {2}" -f $Name, $TimeoutSec, ($Ports -join ',')) -ForegroundColor Red
    return $false
}

switch ($Component) {
    'brain'    { Start-Brain }
    'guardian' { Start-Guardian }
    'core' {
        # Phase 1/2 GO/NO-GO smoke test with EXPLICIT dependency-ordered startup.
        # Order — each step is gated on its bind ports becoming LISTEN before the next starts:
        #   1. brain        binds sensory PULL + motor PUB  (SWIMMY_PORT_SENSORY, 5556)
        #   2. guardian     binds market PUB + external       (5557, 5559); connects -> brain 15555/5556
        #   3. data_keeper  binds data socket                 (5561);       brain connects -> it
        # ZMQ connect-side sockets reconnect, so this order governs deterministic readiness,
        # not correctness (data_keeper-first would also work, avoiding brain's brief
        # offline-history window — flip the $steps order to prefer that).
        # Components launch DETACHED (Start-Process) so they persist after this script returns.
        $sensory = if ($env:SWIMMY_PORT_SENSORY) { [int]$env:SWIMMY_PORT_SENSORY } else { 5555 }
        $py = Join-Path $env:SWIMMY_HOME '.venv\Scripts\python.exe'
        $pwshExe = (Get-Process -Id $PID).Path        # same PowerShell host (run.ps1 needs PS7 features)
        $steps = @(
            @{ Name = 'brain';       Ports = @($sensory, 5556); Grace = 180 }
            @{ Name = 'guardian';    Ports = @(5557, 5559);     Grace = 60  }
            @{ Name = 'data_keeper'; Ports = @(5561);           Grace = 60  }
        )

        Write-Host '=== CORE ORDERED STARTUP: brain -> guardian -> data_keeper ===' -ForegroundColor Cyan
        foreach ($s in $steps) {
            Write-Host ("  step: {0,-12} gate on {1}" -f $s.Name, ($s.Ports -join ',')) -ForegroundColor DarkCyan
        }
        if ($DryRun) {
            Write-Host "`n[DRY-RUN] no processes launched. Remove -DryRun to start the ordered smoke test." -ForegroundColor Yellow
            break
        }

        foreach ($s in $steps) {
            Write-Host ("[{0}] launching (detached)..." -f $s.Name) -ForegroundColor Cyan
            switch ($s.Name) {
                'brain' {
                    Start-Process -FilePath $pwshExe -WindowStyle Hidden `
                        -ArgumentList @('-NoProfile', '-File', "$PSScriptRoot\run.ps1", '-Component', 'brain', '-SkipDataGuard') | Out-Null
                }
                'guardian' {
                    # -SkipDataGuard: the historical-data guard is a Brain concern; guardian
                    # only needs its ZMQ sockets, so don't let it abort guardian startup.
                    Start-Process -FilePath $pwshExe -WindowStyle Hidden `
                        -ArgumentList @('-NoProfile', '-File', "$PSScriptRoot\run.ps1", '-Component', 'guardian', '-SkipDataGuard') | Out-Null
                }
                'data_keeper' {
                    if (Test-Path -LiteralPath $py) {
                        Start-Process -FilePath $py -WindowStyle Hidden `
                            -ArgumentList @((Join-Path $env:SWIMMY_HOME 'tools\data_keeper.py')) `
                            -RedirectStandardOutput (Join-Path $logDir 'data_keeper.log') `
                            -RedirectStandardError  (Join-Path $logDir 'data_keeper.err.log') | Out-Null
                    } else {
                        Write-Host "  data_keeper venv python missing ($py); skipping" -ForegroundColor Yellow
                    }
                }
            }
            if (-not (Wait-Ports -Ports $s.Ports -TimeoutSec $s.Grace -Name $s.Name)) {
                Write-Host ("[{0}] not ready in {1}s — aborting ordered startup." -f $s.Name, $s.Grace) -ForegroundColor Red
                break
            }
        }

        # Final consolidated 5-port GREEN summary.
        Write-Host "`n=== FULL 5-PORT CHECK ===" -ForegroundColor Cyan
        $allPorts = @($sensory, 5556, 5557, 5559, 5561)
        $green = $true
        foreach ($p in $allPorts) {
            $listening = Get-NetTCPConnection -LocalPort $p -State Listen -ErrorAction SilentlyContinue
            if (-not $listening) { $green = $false }
            $mark = if ($listening) { 'LISTEN  OK' } else { 'DOWN' }
            Write-Host ("  port {0,-5} : {1}" -f $p, $mark) -ForegroundColor ($(if ($listening) { 'Green' } else { 'Red' }))
        }
        Write-Host ($(if ($green) { '>>> FULL 5-PORT GREEN <<<' } else { '>>> NOT ALL GREEN <<<' })) `
            -ForegroundColor ($(if ($green) { 'Green' } else { 'Red' }))
        Write-Host 'Components are detached and keep running. Stop by port owner, e.g.:' -ForegroundColor Yellow
        Write-Host '  15555,5557,5561 | % { Get-NetTCPConnection -LocalPort $_ -State Listen -EA 0 | % { Stop-Process -Id $_.OwningProcess -Force } }' -ForegroundColor DarkYellow
    }
}
